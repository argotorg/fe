#!/usr/bin/env bash
set -euo pipefail

REPO="argotorg/fe"
FE_HOME="${FE_HOME:-$HOME/.fe}"
FE_BIN_DIR="$FE_HOME/bin"
MODIFY_PATH=true
REQUESTED_VERSION=""

# --- Colors ---

setup_colors() {
    if [ -t 1 ]; then
        BOLD="\033[1m"
        GREEN="\033[0;32m"
        YELLOW="\033[0;33m"
        RED="\033[0;31m"
        CYAN="\033[0;36m"
        RESET="\033[0m"
    else
        BOLD=""
        GREEN=""
        YELLOW=""
        RED=""
        CYAN=""
        RESET=""
    fi
}

say() {
    printf "%b\n" "${GREEN}${BOLD}feup${RESET}: $1"
}

warn() {
    printf "%b\n" "${YELLOW}${BOLD}warning${RESET}: $1" >&2
}

err() {
    printf "%b\n" "${RED}${BOLD}error${RESET}: $1" >&2
    exit 1
}

# --- Dependency checks ---

need_cmd() {
    if ! command -v "$1" > /dev/null 2>&1; then
        err "need '$1' (command not found)"
    fi
}

# --- Usage ---

usage() {
    cat <<EOF
feup — The Fe toolchain installer

Usage:
    feup [OPTIONS]

Options:
    -h, --help              Print this message
    -v, --version VERSION   Install a specific version (e.g. v26.0.0-alpha.5)
    --no-modify-path        Don't modify shell profile for PATH
EOF
}

# --- Argument parsing ---

parse_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
            -h|--help)
                usage
                exit 0
                ;;
            -v|--version)
                if [ -z "${2:-}" ]; then
                    err "--version requires a value (e.g. --version v26.0.0-alpha.5)"
                fi
                REQUESTED_VERSION="$2"
                shift
                ;;
            --no-modify-path)
                MODIFY_PATH=false
                ;;
            *)
                warn "unknown option: $1"
                usage
                exit 1
                ;;
        esac
        shift
    done
}

# --- Platform detection ---

detect_platform() {
    local os arch

    os="$(uname -s)"
    case "$os" in
        Linux*)  OS="linux" ;;
        Darwin*) OS="mac" ;;
        *)       err "unsupported operating system: $os" ;;
    esac

    arch="$(uname -m)"
    case "$arch" in
        x86_64|amd64)
            # On Apple Silicon under Rosetta, uname reports x86_64.
            if [ "$OS" = "mac" ] && sysctl -n sysctl.proc_translated 2>/dev/null | grep -q 1; then
                warn "running under Rosetta 2 — installing native arm64 binary"
                ARCH="arm64"
            else
                ARCH="amd64"
            fi
            ;;
        aarch64|arm64)
            ARCH="arm64"
            ;;
        *)  err "unsupported architecture: $arch" ;;
    esac
}

# --- Version resolution ---

resolve_version() {
    if [ -n "$REQUESTED_VERSION" ]; then
        # Ensure version starts with 'v'
        case "$REQUESTED_VERSION" in
            v*) VERSION="$REQUESTED_VERSION" ;;
            *)  VERSION="v${REQUESTED_VERSION}" ;;
        esac
        return
    fi

    say "fetching latest release..."

    local curl_args=(-sL --fail)
    if [ -n "${GITHUB_TOKEN:-}" ]; then
        curl_args+=(-H "Authorization: token $GITHUB_TOKEN")
    fi

    # Get the most recent release (including pre-releases) via the API
    local api_response
    api_response=$(curl "${curl_args[@]}" "https://api.github.com/repos/${REPO}/releases?per_page=1") || {
        err "failed to fetch release info from GitHub. If rate-limited, set GITHUB_TOKEN"
    }

    if command -v jq >/dev/null 2>&1; then
        VERSION=$(printf '%s' "$api_response" | jq -r 'map(select(.draft == false))[0].tag_name // empty')
    else
        VERSION=$(printf '%s' "$api_response" | grep -o '"tag_name":[[:space:]]*"[^"]*"' | head -1 | sed 's/.*"tag_name":[[:space:]]*"//;s/"//')
    fi

    if [ -z "$VERSION" ]; then
        err "could not determine latest version from GitHub API"
    fi
}

# --- Download ---

download_fe() {
    local asset_name="fe_${OS}_${ARCH}"
    local url="https://github.com/${REPO}/releases/download/${VERSION}/${asset_name}"

    say "installing Fe ${CYAN}${VERSION}${RESET}"
    say "  platform: ${CYAN}${OS}_${ARCH}${RESET}"
    say "  url: ${CYAN}${url}${RESET}"

    mkdir -p "$FE_BIN_DIR"

    local tmp_file
    tmp_file=$(mktemp)
    trap 'rm -f "$tmp_file"' EXIT

    local curl_args=(-L --fail --progress-bar -o "$tmp_file")
    if [ -n "${GITHUB_TOKEN:-}" ]; then
        curl_args+=(-H "Authorization: token $GITHUB_TOKEN")
    fi

    curl "${curl_args[@]}" "$url" || {
        rm -f "$tmp_file"
        err "download failed — check that version '${VERSION}' exists and has an asset for ${OS}_${ARCH}
  url: ${url}"
    }

    chmod +x "$tmp_file"

    # macOS quarantines binaries downloaded via curl; remove the flag so the
    # binary can execute without Gatekeeper killing it (Abort trap: 6).
    if [ "$OS" = "mac" ]; then
        xattr -d com.apple.quarantine "$tmp_file" 2>/dev/null || true
    fi

    mv "$tmp_file" "$FE_BIN_DIR/fe"
    trap - EXIT

    say "  installed to: ${CYAN}${FE_BIN_DIR}/fe${RESET}"
}

# --- Install feup itself ---

install_feup() {
    # Place this script into FE_BIN_DIR so `feup` is available for future updates.
    local target="$FE_BIN_DIR/feup"
    local self
    local script_ref="${VERSION}"
    local script_url
    local fallback_url
    self="$(cd "$(dirname "$0")" 2>/dev/null && pwd)/$(basename "$0")"

    if [ -f "$self" ]; then
        # Running from a file — copy it (unless it IS the target already).
        local self_abs target_abs
        self_abs="$(cd "$(dirname "$self")" && pwd -P)/$(basename "$self")"
        target_abs="$(cd "$(dirname "$target")" 2>/dev/null && pwd -P)/$(basename "$target")" 2>/dev/null || true
        if [ "$self_abs" = "$target_abs" ]; then
            return
        fi
        cp "$self" "$target"
    else
        # Running via pipe (curl | bash) — prefer matching version, then fall back to master.
        script_url="https://raw.githubusercontent.com/${REPO}/${script_ref}/feup/feup.sh"
        fallback_url="https://raw.githubusercontent.com/${REPO}/master/feup/feup.sh"
        say "downloading feup script (${script_ref})..."
        local curl_args=(-fsSL -o "$target")
        if [ -n "${GITHUB_TOKEN:-}" ]; then
            curl_args+=(-H "Authorization: token $GITHUB_TOKEN")
        fi
        if ! curl "${curl_args[@]}" "$script_url"; then
            warn "could not download feup script from ${script_url}"
            if [ "$script_ref" != "main" ]; then
                say "falling back to feup script (master)..."
                if ! curl "${curl_args[@]}" "$fallback_url"; then
                    rm -f "$target"
                    warn "could not download feup script from ${fallback_url} — 'feup' command won't be available"
                    return 0
                fi
            else
                rm -f "$target"
                warn "'feup' command won't be available"
                return 0
            fi
        fi
    fi

    chmod +x "$target"
}

# --- PATH configuration ---

create_env_file() {
    local env_file="$FE_HOME/env"

    cat > "$env_file" <<ENVEOF
# Fe toolchain PATH setup (managed by feup)
case ":\${PATH}:" in
    *":${FE_BIN_DIR}:"*) ;;
    *) export PATH="${FE_BIN_DIR}:\$PATH" ;;
esac
ENVEOF
}

modify_shell_profile() {
    if [ "$MODIFY_PATH" != true ]; then
        return
    fi

    local source_line=". \"$FE_HOME/env\""
    local modified=false
    local shell_name

    shell_name="$(basename "${SHELL:-/bin/sh}")"

    # Determine which profile files to modify
    local profiles=()
    case "$shell_name" in
        zsh)
            profiles+=("$HOME/.zshenv")
            ;;
        bash)
            # Cover both login and interactive shells when these files exist.
            if [ -f "$HOME/.bash_profile" ]; then
                profiles+=("$HOME/.bash_profile")
            fi
            if [ -f "$HOME/.bashrc" ]; then
                profiles+=("$HOME/.bashrc")
            fi
            if [ "${#profiles[@]}" -eq 0 ]; then
                if [ -f "$HOME/.profile" ]; then
                    profiles+=("$HOME/.profile")
                else
                    # No existing profile — create .bashrc
                    profiles+=("$HOME/.bashrc")
                fi
            fi
            ;;
        fish)
            # Fish uses a different syntax
            local fish_conf_dir="$HOME/.config/fish/conf.d"
            mkdir -p "$fish_conf_dir"
            local fish_conf="$fish_conf_dir/fe.fish"
            if [ ! -f "$fish_conf" ] || ! grep -qF "$FE_BIN_DIR" "$fish_conf" 2>/dev/null; then
                cat > "$fish_conf" <<FISHEOF
# Fe toolchain PATH setup (managed by feup)
if not contains "$FE_BIN_DIR" \$PATH
    set -gx PATH "$FE_BIN_DIR" \$PATH
end
FISHEOF
                modified=true
                say "  modified: ${CYAN}${fish_conf}${RESET}"
            fi
            return
            ;;
        *)
            profiles+=("$HOME/.profile")
            ;;
    esac

    # Remove duplicates
    local unique_profiles=()
    local seen=""
    for p in "${profiles[@]}"; do
        case "$seen" in
            *"|$p|"*) ;;
            *)
                seen="${seen}|$p|"
                unique_profiles+=("$p")
                ;;
        esac
    done

    for profile in "${unique_profiles[@]}"; do
        if [ -f "$profile" ] && grep -qF "$FE_HOME/env" "$profile" 2>/dev/null; then
            continue
        fi
        printf '\n# Fe toolchain\n%s\n' "$source_line" >> "$profile"
        modified=true
        say "  modified: ${CYAN}${profile}${RESET}"
    done

    if [ "$modified" = true ]; then
        say ""
        say "restart your shell or run:"
        say "  ${CYAN}source \"$FE_HOME/env\"${RESET}"
    fi
}

# --- Main ---

main() {
    setup_colors
    parse_args "$@"

    need_cmd curl
    need_cmd chmod
    need_cmd mkdir
    need_cmd uname

    printf "\n"
    say "${BOLD}feup${RESET} — the Fe toolchain installer"
    printf "\n"

    detect_platform
    resolve_version
    download_fe
    install_feup
    create_env_file
    modify_shell_profile

    printf "\n"
    say "${GREEN}done!${RESET} Fe ${CYAN}${VERSION}${RESET} is installed at ${CYAN}${FE_BIN_DIR}/fe${RESET}"

    # Verify the installation
    if "$FE_BIN_DIR/fe" --version > /dev/null 2>&1; then
        local installed_version
        installed_version=$("$FE_BIN_DIR/fe" --version 2>&1 || true)
        say "  ${installed_version}"
    fi

    printf "\n"
}

main "$@"

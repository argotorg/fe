use camino::Utf8PathBuf;
use std::{env, path::PathBuf};

fn utf8_path_from_os(value: std::ffi::OsString) -> Option<Utf8PathBuf> {
    Utf8PathBuf::from_path_buf(PathBuf::from(value)).ok()
}

fn env_path(var: &str) -> Option<Utf8PathBuf> {
    env::var_os(var).and_then(utf8_path_from_os)
}

fn join_components(mut base: Utf8PathBuf, components: &[&str]) -> Utf8PathBuf {
    for component in components {
        base.push(component);
    }
    base
}

fn xdg_cache_dir(name: &str) -> Option<Utf8PathBuf> {
    env_path("XDG_CACHE_HOME").map(|path| join_components(path, &["fe", name]))
}

fn unix_home_cache(name: &str) -> Option<Utf8PathBuf> {
    #[cfg(unix)]
    {
        env_path("HOME").map(|path| join_components(path, &[".cache", "fe", name]))
    }
    #[cfg(not(unix))]
    {
        let _ = name;
        None
    }
}

fn windows_local_appdata(name: &str) -> Option<Utf8PathBuf> {
    #[cfg(windows)]
    {
        env_path("LOCALAPPDATA")
            .or_else(|| env_path("USERPROFILE"))
            .map(|path| join_components(path, &["Fe", "cache", name]))
    }
    #[cfg(not(windows))]
    {
        let _ = name;
        None
    }
}

/// Fe's cache directory for `name`: under `FE_CACHE_DIR` if set, else under
/// the user's cache directory, else under the system temp directory.
pub fn cache_dir(name: &str) -> Option<Utf8PathBuf> {
    env_path("FE_CACHE_DIR")
        .map(|path| join_components(path, &[name]))
        .or_else(|| xdg_cache_dir(name))
        .or_else(|| unix_home_cache(name))
        .or_else(|| windows_local_appdata(name))
        .or_else(|| {
            utf8_path_from_os(env::temp_dir().into())
                .map(|path| join_components(path, &["fe", name]))
        })
}

pub fn remote_git_cache_dir() -> Option<Utf8PathBuf> {
    env_path("FE_REMOTE_CACHE_DIR")
        .or_else(|| env_path("FE_CACHE_DIR").filter(|path| path.ends_with("git")))
        .or_else(|| cache_dir("git"))
}

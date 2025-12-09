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

fn xdg_cache_dir() -> Option<Utf8PathBuf> {
    env_path("XDG_CACHE_HOME").map(|path| join_components(path, &["fe", "git"]))
}

fn unix_home_cache() -> Option<Utf8PathBuf> {
    #[cfg(unix)]
    {
        env_path("HOME").map(|path| join_components(path, &[".cache", "fe", "git"]))
    }
    #[cfg(not(unix))]
    {
        None
    }
}

fn windows_local_appdata() -> Option<Utf8PathBuf> {
    #[cfg(windows)]
    {
        env_path("LOCALAPPDATA")
            .or_else(|| env_path("USERPROFILE"))
            .map(|path| join_components(path, &["Fe", "cache", "git"]))
    }
    #[cfg(not(windows))]
    {
        None
    }
}

pub fn remote_git_cache_dir() -> Option<Utf8PathBuf> {
    env_path("FE_REMOTE_CACHE_DIR")
        .or_else(|| {
            env_path("FE_CACHE_DIR").map(|path| {
                if path.ends_with("git") {
                    path
                } else {
                    join_components(path, &["git"])
                }
            })
        })
        .or_else(xdg_cache_dir)
        .or_else(unix_home_cache)
        .or_else(windows_local_appdata)
        .or_else(|| {
            utf8_path_from_os(env::temp_dir().into())
                .map(|path| join_components(path, &["fe", "git"]))
        })
}

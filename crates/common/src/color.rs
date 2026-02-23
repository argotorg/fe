use std::{
    env,
    io::{self, IsTerminal},
    sync::atomic::{AtomicU8, Ordering},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorPreference {
    Auto,
    Always,
    Never,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorTarget {
    Stdout,
    Stderr,
}

// 0 = auto, 1 = always, 2 = never
static COLOR_PREFERENCE: AtomicU8 = AtomicU8::new(0);

pub fn set_color_preference(preference: ColorPreference) {
    let value = match preference {
        ColorPreference::Auto => 0,
        ColorPreference::Always => 1,
        ColorPreference::Never => 2,
    };
    COLOR_PREFERENCE.store(value, Ordering::Relaxed);
}

pub fn color_preference() -> ColorPreference {
    match COLOR_PREFERENCE.load(Ordering::Relaxed) {
        1 => ColorPreference::Always,
        2 => ColorPreference::Never,
        _ => ColorPreference::Auto,
    }
}

pub fn should_colorize(target: ColorTarget) -> bool {
    match color_preference() {
        ColorPreference::Always => true,
        ColorPreference::Never => false,
        ColorPreference::Auto => should_colorize_auto(target),
    }
}

fn normalize_env(value: Result<String, env::VarError>) -> Option<bool> {
    match value {
        Ok(string) => Some(string != "0"),
        Err(_) => None,
    }
}

fn should_colorize_auto(target: ColorTarget) -> bool {
    if normalize_env(env::var("CLICOLOR_FORCE")) == Some(true) {
        return true;
    }

    if normalize_env(env::var("NO_COLOR")).is_some() {
        return false;
    }

    let clicolor = normalize_env(env::var("CLICOLOR")).unwrap_or(true);
    if !clicolor {
        return false;
    }

    match target {
        ColorTarget::Stdout => io::stdout().is_terminal(),
        ColorTarget::Stderr => io::stderr().is_terminal(),
    }
}

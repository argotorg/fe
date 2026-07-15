use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OptLevel {
    O0,
    O1,
    Os,
    #[default]
    O2,
}

impl std::str::FromStr for OptLevel {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(OptLevel::O0),
            "1" => Ok(OptLevel::O1),
            "s" => Ok(OptLevel::Os),
            "2" => Ok(OptLevel::O2),
            _ => Err(format!(
                "unknown optimization level: {s} (expected '0', '1', '2', or 's')"
            )),
        }
    }
}

impl fmt::Display for OptLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OptLevel::O0 => write!(f, "0"),
            OptLevel::O1 => write!(f, "1"),
            OptLevel::Os => write!(f, "s"),
            OptLevel::O2 => write!(f, "2"),
        }
    }
}

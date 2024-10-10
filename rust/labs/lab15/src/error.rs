use std::fmt;

/// Enum for errors that can occur when making a move
#[derive(Debug, PartialEq)]
pub enum MoveError {
    OutOfBounds,
    Occupied,
    KoFight,
    SelfCapture,
    Suicide,
}

impl fmt::Display for MoveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MoveError::OutOfBounds => write!(f, "Move is out of bounds!"),
            MoveError::Occupied => write!(f, "Position is already occupied!"),
            MoveError::KoFight => write!(f, "Move violates the Ko rule!"),
            MoveError::SelfCapture => write!(f, "Move would result in self-capture!"),
            MoveError::Suicide => write!(f, "Move would result in suicide!"),
        }
    }
}

/// Enum for erorrs that can occur when making a command in the main entry
#[derive(Debug, PartialEq)]
pub enum CommandError<'a> {
    ShouldBeLen(usize),
    ShouldBeFormat(&'a str),
    InvalidStoneType,
    NonExistingStone,
    Unknown,
}

impl fmt::Display for CommandError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CommandError::ShouldBeLen(len) => {
                write!(f, "Command shouldn't be more than {len} fields!")
            }

            CommandError::ShouldBeFormat(s) => write!(f, "Command should be in the form '{s}'!"),
            CommandError::InvalidStoneType => write!(f, "Stone type should be 'B' or 'W'!"),
            CommandError::NonExistingStone => write!(f, "Stone doesn't exists at given point!"),
            CommandError::Unknown => write!(f, "Unknom command!"),
        }
    }
}

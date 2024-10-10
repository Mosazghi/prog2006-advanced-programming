use crate::error::CommandError;
use std::fmt::Display;

/// Enum for prased command
#[derive(Debug, PartialEq)]
pub enum ParsedCoordinate<'a> {
    Move {
        x: usize,
        y: usize,
        stone_type: Option<Stone>,
    },
    Error(CommandError<'a>),
}

/// Enum for game mode
#[derive(Debug, Clone, PartialEq)]
pub enum GameMode {
    PlayerVsPlayer,
    PlayerVsComputer,
}

#[derive(Debug, Clone, PartialEq, Default, Copy)]
/// Enum representing the type (color) of a stone on the board
pub enum Stone {
    White,
    Black,
    #[default]
    Empty,
}

impl Display for Stone {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stone::White => write!(f, "W"),
            Stone::Black => write!(f, "B"),
            Stone::Empty => write!(f, "."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stone_type_display() {
        assert_eq!(format!("{}", Stone::White), "W");
        assert_eq!(format!("{}", Stone::Black), "B");
        assert_eq!(format!("{}", Stone::Empty), ".");
    }

    #[test]
    fn test_stone_type_default() {
        let default_stone: Stone = Default::default();
        assert_eq!(default_stone, Stone::Empty);
    }
}

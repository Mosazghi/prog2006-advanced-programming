use crate::board::{Board, BoardType};
use crate::enums::{GameMode, ParsedCoordinate, Stone};
use crate::error::{CommandError, MoveError};
use crate::gameplayer::GamePlayer;
use crate::logic::{explore_neighbors_freedom, find_best_move_for_freedom, get_best_move};
use crate::rule::{is_empty_point, is_valid_move};
use crate::structs::Cmd;
use crate::util::{format_points, parse_coordinate};
use std::{collections::BTreeMap, usize, vec};

/// The game struct - holds access to everything
/// # Fields
/// * `size` - The size of the board
/// * `board` - The board
/// * `board_states` - The board states
#[derive(Debug, Clone)]
pub struct Game {
    pub board: Board,
    board_states: Vec<BoardType>,
}

impl Game {
    pub fn new(size: Option<(usize, usize)>) -> Self {
        let size = size.unwrap_or((9, 9));

        Self {
            board: Board::new(Some(size)),
            board_states: vec![BTreeMap::new()],
        }
    }

    /// Get the last board state
    pub fn get_last_board_state(&self) -> &BoardType {
        self.board_states.last().unwrap() // We'll always have at least one board state
    }

    /// Push the current board state to the board states
    pub fn push_to_board_history(&mut self) {
        self.board_states.push(self.board.board.clone());
    }

    /// Make a move on the board and update
    /// NOTE: I have defined the function here due to the need to save the board state
    ///
    /// # Arguments
    /// * `x` - The x-coordinate
    /// * `y` - The y-coordinate
    /// * `stone_type` - The type of stone to place
    /// # Returns
    /// * `Result<usize, String>` - The number of points scored based on the number of removed (captured) stones
    pub fn make_move(
        &mut self,
        x: usize,
        y: usize,
        stone_type: &Stone,
    ) -> Result<usize, MoveError> {
        is_valid_move(&self.board, self.get_last_board_state(), x, y, stone_type)?;

        self.push_to_board_history();
        let points = self.board.update_board(x, y, stone_type);

        self.display_board();
        Ok(points)
    }

    /// Run the game based on a given command (main entry)
    /// # Arguments
    /// * `command` - The command to run
    // TODO: Split into smaller functions?
    pub fn run(&mut self, command: &str) {
        let command = Cmd::new(command);

        match command.first() {
            "R" => self.read_from_sgf(),
            "W" => self.write_to_sgf(),
            "D" => self.display_board(),
            "S" => self.show_stats(),
            "C" => self.play_game(GameMode::PlayerVsComputer),
            "P" => self.play_game(GameMode::PlayerVsPlayer),
            "TB" => self.find_best_move(&Stone::Black),
            "TW" => self.find_best_move(&Stone::White),
            "E" => self.evaluate_best_freedom(&command),
            "F" => self.evaluate_number_freedoms(&command),
            "M" => self.make_move_command(&command),
            "Q" => self.quit(),
            _ => self.unknown_command(),
        }
    }

    /// Read from a given SGF file and update the board
    fn read_from_sgf(&mut self) {
        let mut file_path = String::new();

        println!("Enter the file path including '.sgf': ");
        std::io::stdin().read_line(&mut file_path).unwrap();

        if let Err(e) = self.board.read_from_sgf(file_path.as_str()) {
            println!("Error reading SGF file: {}", e);
        }
    }

    /// Write to an SGF file
    fn write_to_sgf(&mut self) {
        self.board.write_to_sgf();
    }

    /// Display the board
    pub fn display_board(&self) {
        self.board.display();
    }

    /// Show the statistics of the game
    fn show_stats(&self) {
        self.board.stats();
    }

    /// Play a game
    fn play_game(&mut self, mode: GameMode) {
        let mut game_player = GamePlayer::new(self, mode);
        game_player.play_game();
    }

    /// Exit the program
    fn quit(&self) {
        std::process::exit(0)
    }

    fn unknown_command(&self) {
        eprintln!("{}", CommandError::Unknown)
    }

    /// Find the best move for a given stone type
    /// # Arguments
    /// * `stone_type` - The stone type
    fn find_best_move(&self, stone_type: &Stone) {
        let best_move = get_best_move(&self.board.clone(), self.get_last_board_state(), stone_type);
        println!(
            "Best move for {:?} given current board state: {}",
            stone_type,
            format_points(best_move.0, best_move.1)
        );
    }

    /// Evaluate the best move to extend liberties for a given group
    /// # Arguments
    /// * `coordinate` - The coordinate
    fn evaluate_best_freedom(&self, coordinate: &Cmd) {
        match parse_coordinate(coordinate) {
            ParsedCoordinate::Move { x, y, .. } => {
                if is_empty_point(&self.board, x, y) {
                    eprintln!("{}", CommandError::NonExistingStone);
                } else {
                    let best_move = find_best_move_for_freedom(&self.board, x, y);
                    println!(
                        "Best move to extend liberties for group at {} is: {}",
                        format_points(x, y),
                        format_points(best_move.0, best_move.1)
                    );
                }
            }
            ParsedCoordinate::Error(e) => eprintln!("{}", e),
        }
    }

    /// Evaluate the number of liberties for a given group
    /// # Arguments
    /// * `coordinate` - The coordinate
    fn evaluate_number_freedoms(&self, coordinate: &Cmd) {
        match parse_coordinate(coordinate) {
            ParsedCoordinate::Move { x, y, .. } => {
                if is_empty_point(&self.board, x, y) {
                    eprintln!("{}", CommandError::NonExistingStone);
                } else {
                    let liberties = explore_neighbors_freedom(&self.board, x, y);
                    println!(
                        "Number of liberties for group at {} is: {}",
                        format_points(x, y),
                        liberties.len()
                    );
                }
            }
            ParsedCoordinate::Error(e) => eprintln!("{}", e),
        }
    }

    /// Makes a move on the board
    /// # Arguments
    /// * `coordinate` - The coordinate
    fn make_move_command(&mut self, coordinate: &Cmd) {
        match parse_coordinate(coordinate) {
            ParsedCoordinate::Move { x, y, stone_type } => {
                if let Some(stone_type) = stone_type {
                    if let Err(e) = self.make_move(x, y, &stone_type) {
                        eprintln!("{}", e);
                    } else {
                        println!(
                            "Move made at {} with stone type {:?}",
                            format_points(x, y),
                            stone_type
                        );
                    }
                } else {
                    eprintln!("{}", CommandError::InvalidStoneType);
                }
            }
            ParsedCoordinate::Error(e) => eprintln!("{}", e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_move() {
        let mut game = Game::new(Some((9, 9)));
        game.board.update_board(0, 0, &Stone::Black);
        game.board.update_board(0, 1, &Stone::White);
        let result = game.make_move(1, 0, &Stone::White);

        assert_eq!(result, Ok(1));
    }
}

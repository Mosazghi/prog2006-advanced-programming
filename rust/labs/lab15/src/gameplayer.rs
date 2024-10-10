use crate::enums::{GameMode, ParsedCoordinate, Stone};
use crate::error::MoveError;
use crate::game::Game;
use crate::logic::get_best_move;
use crate::structs::{Cmd, Player};
use crate::util::{parse_coordinate, read_name};
use std::io;

/// Struct representing a the gameplayer (for two specific commands for game.rs)
/// # Fields
/// * `game` - The game
/// * `mode` - The game mode
pub struct GamePlayer<'a> {
    game: &'a mut Game,
    mode: GameMode,
}

impl<'a> GamePlayer<'a> {
    pub fn new(game: &'a mut Game, mode: GameMode) -> Self {
        GamePlayer { game, mode }
    }

    /// Main entry point for playing the game
    pub fn play_game(&mut self) {
        let mut is_player_turn = true;
        let (mut player1, mut player2) = self.setup_game();
        self.game_loop(&mut player1, &mut player2, &mut is_player_turn);
    }

    /// The main game loop
    /// # Arguments
    /// * `current_player` - The current player
    /// * `opponent` - The opponent
    /// * `is_player_turn` - Whether it is the player's turn (relevant when in player vs. computer)
    fn game_loop<'b>(
        &mut self,
        mut current_player: &'b mut Player,
        mut opponent: &'b mut Player,
        is_player_turn: &mut bool,
    ) {
        self.game.display_board();

        loop {
            println!("{}'s turn", current_player.name);

            let mut command = String::new();
            self.read_coordinate(&mut command, is_player_turn);

            let mut coordinate = Cmd::new(command.as_str());

            // Dummy insert value to please the parse_coordinate function
            coordinate.insert(0, "_");

            match parse_coordinate(&coordinate) {
                ParsedCoordinate::Move { x, y, .. } => {
                    if let Err(e) = self.process_coordinate(x, y, current_player) {
                        eprintln!("{}", e);
                        continue;
                    }
                }
                ParsedCoordinate::Error(e) => {
                    eprintln!("{}", e);
                    continue;
                }
            }

            if self.is_game_over(current_player) {
                println!("{} wins!", current_player.name);
                break;
            }

            std::mem::swap(&mut current_player, &mut opponent);

            if self.mode != GameMode::PlayerVsPlayer {
                *is_player_turn = !*is_player_turn;
            }
        }
    }

    /// Setup the game
    /// # Returns
    /// * `Player` - The first player
    /// * `Player` - The second player
    fn setup_game(&mut self) -> (Player, Player) {
        self.game.board.clear_board();

        println!("Name of player 1 (Black): ");
        let player1 = Player::new(read_name(), Stone::Black);

        let player2 = match self.mode {
            GameMode::PlayerVsPlayer => {
                println!("Name of player 2 (White): ");
                Player::new(read_name(), Stone::White)
            }
            GameMode::PlayerVsComputer => Player::new("Computer".to_string(), Stone::White),
        };
        (player1, player2)
    }
    /// Process the coordinate
    /// # Arguments
    /// * `x` - The x coordinate
    /// * `y` - The y coordinate
    /// * `current_player` - The current player
    /// # Returns
    /// * `Result<(), MoveError>` - The result of the move
    fn process_coordinate(
        &mut self,
        x: usize,
        y: usize,
        current_player: &mut Player,
    ) -> Result<(), MoveError> {
        match self.game.make_move(x, y, &current_player.stone_type) {
            Ok(points) => {
                current_player.score += points;

                if points > 0 {
                    println!(
                        "\n{}'s score: {} [+{}]",
                        current_player.name, current_player.score, points
                    );
                }
            }
            Err(e) => return Err(e),
        };
        Ok(())
    }

    /// Read the coordinate
    /// # Arguments
    /// * `coordinate` - The coordinate
    /// * `is_player_turn` - Whether it is the player's turn
    fn read_coordinate(&self, coordinate: &mut String, is_player_turn: &bool) {
        if *is_player_turn || self.mode == GameMode::PlayerVsPlayer {
            io::stdin().read_line(coordinate).unwrap();
        } else {
            // slepp for 1 second
            std::thread::sleep(std::time::Duration::from_secs(1));
            let (x, y) = get_best_move(
                &self.game.board,
                self.game.get_last_board_state(),
                &Stone::White,
            );
            *coordinate = format!(
                "{} {}",
                char::from_u32('a' as u32 + x as u32).unwrap(),
                char::from_u32('a' as u32 + y as u32).unwrap()
            );
            println!("Computer's move: {}\n", coordinate);
        }
    }

    /// Check if the game is over
    /// # Arguments
    /// * `current_player` - The current player
    /// TODO: Add more conditions for game over (pass, resign etc.)?
    fn is_game_over(&self, current_player: &Player) -> bool {
        current_player.score >= 2
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_game_over() {
        let mut game = Game::new(Some((9, 9)));
        let game_player = GamePlayer::new(&mut game, GameMode::PlayerVsPlayer);
        let player = Player::new("Player".to_string(), Stone::Black);
        assert!(!game_player.is_game_over(&player));
    }
}

use crate::game::Game;
use crate::util::print_menu;

pub mod board;
pub mod enums;
pub mod error;
pub mod game;
pub mod gameplayer;
pub mod logic;
pub mod rule;
pub mod structs;
pub mod util;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut game = Game::new(Some((9, 9)));

    println!("\n------ GO GAME - lab15 ------\n");

    loop {
        print_menu();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;
        game.run(&input);
    }
}

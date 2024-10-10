use crate::enums::Stone;
use crate::logic::{count_groups, explore_neighbors_freedom};
use crate::util::{parse_board_size_from_sgf, parse_moves_from_sgf, serialize_board_to_sgf};
use sgf_parse::go::parse;
use std::{collections::BTreeMap, fs};

// Use of BTreeMap instead of HashMap to maintain order when serializing to SGF file
pub type BoardType = BTreeMap<(usize, usize), Stone>;

// TODO: Encapsulate the board?
#[derive(Debug, Clone)]
pub struct Board {
    pub board: BoardType,
    pub size: (usize, usize),
}

impl Board {
    pub fn new(size: Option<(usize, usize)>) -> Self {
        let size = size.unwrap_or((9, 9));
        let board = BTreeMap::new();
        Self { board, size }
    }

    pub fn set_stone(&mut self, x: usize, y: usize, stone: Stone) {
        self.board.insert((x, y), stone);
    }

    // TODO: Return option instead?
    pub fn get_stone(&self, x: usize, y: usize) -> &Stone {
        self.board.get(&(x, y)).unwrap_or(&Stone::Empty)
    }

    pub fn get_stone_mut(&mut self, x: usize, y: usize) -> &mut Stone {
        self.board.get_mut(&(x, y)).unwrap()
    }

    /// Update the board with a new stone and remove dead (captured) groups
    /// # Arguments
    /// * `x` - The x-coordinate of the stone
    /// * `y` - The y-coordinate of the stone
    /// * `stone_type` - The type of stone to place
    /// # Returns
    /// * `usize` - The number of points scored based on the number of removed (captured) stones
    pub fn update_board(&mut self, x: usize, y: usize, stone_type: &Stone) -> usize {
        self.set_stone(x, y, *stone_type);
        let points = self.remove_dead_groups(Some((x, y)));
        // to remove the empty stones that occur from `remove_dead_groups`-function
        self.board.retain(|_, v| *v != Stone::Empty);
        points
    }

    /// Remove dead (captured) groups (stones) from the board
    /// # Arguments
    /// * `to_skip` - The coordinates of a stone to skip
    /// # Returns
    /// * `usize` - The number of points scored based on the number of removed (captured) stones
    pub fn remove_dead_groups(&mut self, to_skip: Option<(usize, usize)>) -> usize {
        let mut to_remove: Vec<(usize, usize)> = vec![];

        self.board.iter().for_each(|(&(x, y), _)| {
            let liberties = explore_neighbors_freedom(self, x, y);
            if liberties.is_empty() {
                to_remove.push((x, y));
            }
        });

        let points = to_remove.len();

        // Such that we don't accidentally remove the stone we just placed
        if let Some((x, y)) = to_skip {
            for (nx, ny) in to_remove {
                if nx != x || ny != y {
                    self.update_board(nx, ny, &Stone::Empty);
                }
            }
        }

        points
    }

    /// Clears the board
    pub fn clear_board(&mut self) {
        self.board.clear();
    }

    /// Count the number of stones of a given type
    /// # Arguments
    /// * `stone_type` - The type of stone to count
    /// # Returns
    /// * `usize` - The number of stones of the given type
    fn count_type(&self, stone_type: &Stone) -> usize {
        self.board.values().filter(|&s| s == stone_type).count()
    }

    /// Display the statistics of the game
    pub fn stats(&self) {
        println!("\tBoard size: {:?}x{:?}", self.size.0, self.size.1);
        println!("\tWhite stones: {}", self.count_type(&Stone::White));
        println!("\tBlack stones: {}\n", self.count_type(&Stone::Black));
        println!("Total number of groups: {}", count_groups(self));
    }

    /// Display the board
    pub fn display(&self) {
        print!("\t    ");
        for i in 0..self.size.0 {
            if let Some(c) = char::from_u32('A' as u32 + i as u32) {
                print!("{} ", c);
            }
        }
        println!();
        for y in 0..self.size.0 {
            print!("\t{}   ", y + 1);

            for x in 0..self.size.1 {
                let stone = self.board.get(&(x, y)).unwrap_or(&Stone::Empty);
                print!("{} ", *stone);
            }
            println!();
        }
    }

    // Read from a given SGF file
    // # Arguments
    // * `file_path` - The path to the SGF file
    // # Returns
    // * `Result<(), io::Error>` - The result of the operation
    pub fn read_from_sgf(&mut self, file_path: &str) -> Result<(), std::io::Error> {
        let file_path = file_path.trim();
        self.clear_board();
        self.init_from_sgf(file_path)?;
        println!("\nBoard initialized from SGF file: {}", file_path);
        Ok(())
    }

    /// Initialize the board from a given SGF file
    /// # Arguments
    /// * `sgf_path` - The path to the SGF file
    /// # Returns
    /// * `Result<(), io::Error>` - The result of the operation
    pub fn init_from_sgf(&mut self, file_path: &str) -> Result<(), std::io::Error> {
        let sgf = std::fs::read_to_string(file_path)?;
        let node = &parse(&sgf).unwrap()[0];
        parse_moves_from_sgf(self, node);
        self.size = parse_board_size_from_sgf(sgf.as_str());

        Ok(())
    }

    /// Write the board to an SGF file
    pub fn write_to_sgf(&self) {
        if self.board.is_empty() {
            eprintln!("\nBoard is empty!");
            return;
        }

        let contents = serialize_board_to_sgf(self);
        fs::write("output.sgf", contents).expect("Unable to write file");
        println!("\nBoard written to SGF file: 'output.sgf'");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rule::is_empty_point;
    #[test]
    fn test_count_type() {
        let mut board = Board::new(Some((9, 9)));
        board.update_board(0, 0, &Stone::Black);
        board.update_board(1, 0, &Stone::Black);
        board.update_board(2, 0, &Stone::Black);
        board.update_board(3, 0, &Stone::White);
        board.update_board(4, 0, &Stone::White);

        assert_eq!(board.count_type(&Stone::Black), 3);
    }

    #[test]
    fn test_update_board() {
        let mut board = Board::new(Some((9, 9)));
        let moves_to_make = [
            ((0, 0), Stone::White),
            ((7, 0), Stone::White),
            ((7, 1), Stone::White),
            ((8, 2), Stone::White),
            ((1, 0), Stone::Black),
            ((0, 1), Stone::Black),
            ((8, 0), Stone::Black),
            ((8, 1), Stone::White),
        ];
        // W B . . . . . W B
        // B . . . . . . W W
        // . . . . . . . . W

        for (p, stone_type) in moves_to_make.iter() {
            board.update_board(p.0, p.1, stone_type);
        }

        assert!(is_empty_point(&board, 0, 0));
        assert!(is_empty_point(&board, 8, 0));
    }
}

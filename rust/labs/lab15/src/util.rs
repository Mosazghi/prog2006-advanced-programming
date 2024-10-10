use crate::board::Board;
use crate::enums::{ParsedCoordinate, Stone};
use crate::error::CommandError;
use crate::structs::Cmd;
use sgf_parse::{
    go::{parse, Move, Prop},
    SgfNode,
};
use std::io::{self, Write};

/// Function to parse a letter to a digit
/// # Arguments
/// * `cord` - character to parse
/// # Returns
/// * Option<usize> - Some(usize) if the char is a letter, None otherwise
pub fn parse_letter_to_digit(cord: &char) -> Option<usize> {
    cord.is_ascii_alphabetic()
        .then(|| (cord.to_ascii_lowercase() as u32 - 'a' as u32) as usize)
}

/// Function to convert coordinates to points
/// # Arguments
/// * `x` - x coordinate
/// * `y` - y coordinate
/// # Returns
/// * String - points
pub fn format_points(x: usize, y: usize) -> String {
    format!("{}{}", (x as u8 + b'a') as char, (y as u8 + b'a') as char)
}

/// Function to read a name from the user
pub fn read_name() -> String {
    io::stdin().lines().next().unwrap().unwrap()
}

/// Parse the coordinate (and stone type)
/// # Arguments
/// * `coordinate` - The coordinate
/// # Returns
/// * `ParsedCoordinate` - The result of the parsing
pub fn parse_coordinate(coordinate: &Cmd) -> ParsedCoordinate {
    let args = coordinate.all();
    if args.len() < 3 {
        return ParsedCoordinate::Error(CommandError::ShouldBeLen(3));
    }

    let x = parse_letter_to_digit(&args[1].chars().next().unwrap_or('z'));
    let y = parse_letter_to_digit(&args[2].chars().next().unwrap_or('z'));

    match (x, y) {
        (Some(x), Some(y)) => {
            let stone_type = if args.len() > 3 {
                match args[3].as_str() {
                    "W" => Some(Stone::White),
                    "B" => Some(Stone::Black),
                    _ => return ParsedCoordinate::Error(CommandError::InvalidStoneType),
                }
            } else {
                None
            };

            ParsedCoordinate::Move { x, y, stone_type }
        }
        _ => ParsedCoordinate::Error(CommandError::ShouldBeFormat("<a-r> <a-r> (<W OR B>)")),
    }
}

/// Read moves from the sgf SgfNode
/// # Arguments
/// * `board` - The board to update
/// * `moves` - The moves to parse
pub fn parse_moves_from_sgf(board: &mut Board, moves: &SgfNode<Prop>) {
    let moves: Vec<Prop> = moves
        .main_variation()
        .map(|n| {
            n.get_property("B")
                .or_else(|| n.get_property("W"))
                .unwrap_or(&Prop::Invalid("Not valid B or W move".to_string(), vec![]))
                .clone()
        })
        .collect();

    for m in moves.iter() {
        match m {
            Prop::B(Move::Move(p)) | Prop::W(Move::Move(p)) => {
                let to_insert = match m {
                    Prop::B(_) => Stone::Black,
                    Prop::W(_) => Stone::White,
                    _ => Stone::Empty,
                };

                board.update_board(p.x as usize, p.y as usize, &to_insert);
            }
            _ => {}
        }
    }
}

/// Parse the board size from the SGF file
/// # Arguments
/// * `sgf` - The SGF file content as a string
/// # Returns
/// * `(usize, usize)` - The board size
pub fn parse_board_size_from_sgf(sgf: &str) -> (usize, usize) {
    let node = parse(sgf).unwrap().into_iter().next().unwrap();
    let board_size = match node.get_property("SZ") {
        Some(Prop::SZ(size)) => *size,
        _ => (9, 9),
    };

    (board_size.0 as usize, board_size.1 as usize)
}

/// Serialize the board to SGF format
/// WARNING: This does NOT preserve the order of the moves (future work?)
///
/// # Arguments
/// * `board` - The board to serialize
/// # Returns
/// * String - The SGF formatted string
pub fn serialize_board_to_sgf(board: &Board) -> String {
    let mut components = Vec::new();

    components.push(format!("(;SZ[{}:{}]", board.size.0, board.size.1));

    board.board.iter().for_each(|(&(x, y), &stone)| {
        let point = format_points(x, y);
        let stone = match stone {
            Stone::White => "W",
            Stone::Black => "B",
            _ => unreachable!(),
        };

        components.push(format!(";{}[{}]", stone, point));
    });

    components.push(")".to_string());
    components.join("")
}

/// Function to print the menu
pub fn print_menu() {
    println!("\nPossible commands:");
    println!("\n\tR <file>\t\t Read SGF file");
    println!("\tW\t\t\t Write SGF file");
    println!("\tD\t\t\t Display board");
    println!("\tS\t\t\t Board stats");
    println!("\tP\t\t\t Human vs Human");
    println!("\tC\t\t\t Human vs Computer");
    println!("\tTB\t\t\t Best move for Black player");
    println!("\tTW\t\t\t Best move for White player");
    println!("\tE <coordinates>\t\t Move the extends freedom the most");
    println!("\tM <coordinates> <W / B>\t Make move");
    println!("\tF <coordinates>\t\t Number of freedoms");
    println!("\tQ\t\t\t Quit");
    print!("\n> ");
    std::io::stdout().flush().unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_board_size_from_sgf() {
        let size = parse_board_size_from_sgf("(;SZ[13:13];B[de])");
        assert_eq!(size, (13, 13));
        let size = parse_board_size_from_sgf("(;SZ[9:9];B[de])");
        assert_eq!(size, (9, 9));
    }
    #[test]
    fn test_serialize_board_to_sgf() {
        let mut board = Board::new(Some((9, 9)));

        board.update_board(0, 0, &Stone::Black);
        board.update_board(0, 1, &Stone::White);
        board.update_board(8, 8, &Stone::White);

        let sgf = serialize_board_to_sgf(&board);

        assert_eq!(sgf, "(;SZ[9:9];B[aa];W[ab];W[ii])");
    }

    #[test]
    fn test_parse_coordinate() {
        let cmd = Cmd::new("M a b B");
        let coord = parse_coordinate(&cmd);
        assert_eq!(
            coord,
            ParsedCoordinate::Move {
                x: 0,
                y: 1,
                stone_type: Some(Stone::Black)
            }
        );

        let cmd = Cmd::new("M a b");
        let coord = parse_coordinate(&cmd);
        assert_eq!(
            coord,
            ParsedCoordinate::Move {
                x: 0,
                y: 1,
                stone_type: None
            }
        );

        let cmd = Cmd::new("M a b W");
        let coord = parse_coordinate(&cmd);
        assert_eq!(
            coord,
            ParsedCoordinate::Move {
                x: 0,
                y: 1,
                stone_type: Some(Stone::White)
            }
        );

        let cmd = Cmd::new("M a b C");
        let coord = parse_coordinate(&cmd);

        assert_eq!(
            coord,
            ParsedCoordinate::Error(CommandError::InvalidStoneType)
        );

        let cmd = Cmd::new("M a");
        let coord = parse_coordinate(&cmd);
        assert_eq!(coord, ParsedCoordinate::Error(CommandError::ShouldBeLen(3)));
    }

    #[test]
    fn test_parse_letter_to_digit() {
        assert_eq!(parse_letter_to_digit(&'a'), Some(0));
        assert_eq!(parse_letter_to_digit(&'A'), Some(0));
        assert_eq!(parse_letter_to_digit(&'z'), Some(25));
        assert_eq!(parse_letter_to_digit(&'Z'), Some(25));
        assert_eq!(parse_letter_to_digit(&'1'), None);
        assert_eq!(parse_letter_to_digit(&'!'), None);
    }

    #[test]
    fn test_format_points() {
        assert_eq!(format_points(0, 0), "aa");
        assert_eq!(format_points(1, 1), "bb");
        assert_eq!(format_points(2, 2), "cc");
        assert_eq!(format_points(3, 3), "dd");
    }
}

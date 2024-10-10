use crate::board::{Board, BoardType};
use crate::enums::Stone;
use crate::error::MoveError;
use crate::logic::explore_neighbors_freedom;

/// Check if a given coordinate is within the board
/// # Arguments
/// * `size` - The size of the board
/// * `x` - The x-coordinate
/// * `y` - The y-coordinate
/// # Returns
/// * `bool` - Whether the coordinate is within the board
fn is_valid_range(size: &(usize, usize), x: usize, y: usize) -> bool {
    // Since the we are operating using usize, we do not need to check for negative values
    x < size.0 && y < size.1
}

/// Check if a given move is a 'ko fight' (or more clearly, repeated move)
/// # Arguments
/// * `curr_board` - The current board state
/// * `prev_board` - The previous board state
/// * `x` - The x-coordinate
/// * `y` - The y-coordinate
/// * `stone_type` - The type of stone to place
/// # Returns
/// * `bool` - Whether the move is a 'ko fight' (repeated move)
fn is_ko_fight(
    curr_board: &Board,
    prev_board: &BoardType,
    x: usize,
    y: usize,
    stone_type: &Stone,
) -> bool {
    let mut board_copy = curr_board.clone();
    board_copy.update_board(x, y, stone_type); // simulate placing the stone
    board_copy.board == *prev_board
}

/// Check if a given move is a 'self-capture' move (deadlock)
/// # Arguments
/// * `board` - The current board state
/// * `x` - The x-coordinate
/// * `y` - The y-coordinate
/// * `stone_type` - The type of stone to place
/// # Returns
/// * `bool` - Whether the move is a 'self-capture' move
fn is_suicide_move(board: &Board, x: usize, y: usize, stone_type: &Stone) -> bool {
    let mut board_copy = board.clone();
    board_copy.update_board(x, y, stone_type);
    let neighbors = explore_neighbors_freedom(&board_copy, x, y);

    neighbors.is_empty() // can the stone breath?
}

/// Check if a given point is empty
/// # Arguments
/// * `board` - The current board state
/// * `x` - The x-coordinate
/// * `y` - The y-coordinate
/// # Returns
/// * `bool` - Whether the point is Empty
pub fn is_empty_point(board: &Board, x: usize, y: usize) -> bool {
    !board.board.contains_key(&(x, y))
}

/// Checks if the move is suicidal, i.e. trying to self-capture same stone type
/// # Arguments
/// * `board` - The current board state
/// * `x` - x-coordinate
/// * `y` - y-coordinate
/// # Returns
/// * `bool` - Wheter it's a self-capture or not
pub fn is_self_capture_move(board: &Board, x: usize, y: usize, stone_type: &Stone) -> bool {
    let mut copy = board.clone();
    let mut flag = false;

    copy.set_stone(x, y, *stone_type);

    copy.board.iter().for_each(|(&(nx, ny), &stone)| {
        if nx != x || ny != y {
            let liberties = explore_neighbors_freedom(&copy, nx, ny);
            // This means we are trying to capture an
            // ally, which is not ideal
            if liberties.is_empty() && stone == *stone_type {
                flag = true;
            }
        }
    });

    flag
}

/// Check if a given move is valid based on few conditions
/// # Arguments
/// * `board` - The current board state
/// * `prev_board` - The previous board
/// * `x` - The x-coordinate
/// * `y` - The y-coordinate
/// * `stone_type` - The type of stone to place
/// # Returns
/// * `Result<(), &str>` - The result of the operation
pub fn is_valid_move(
    board: &Board,
    prev_board: &BoardType,
    x: usize,
    y: usize,
    stone_type: &Stone,
) -> Result<(), MoveError> {
    if !is_valid_range(&board.size, x, y) {
        return Err(MoveError::OutOfBounds);
    }
    if !is_empty_point(board, x, y) {
        return Err(MoveError::Occupied);
    }

    if is_ko_fight(board, prev_board, x, y, stone_type) {
        return Err(MoveError::KoFight);
    }

    if is_self_capture_move(board, x, y, stone_type) {
        return Err(MoveError::SelfCapture);
    }

    if is_suicide_move(board, x, y, stone_type) {
        return Err(MoveError::Suicide);
    }

    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::enums::Stone;
    use crate::game::Game;
    #[test]
    fn test_is_empty_point() {
        let mut board = Board::new(Some((9, 9)));
        assert!(is_empty_point(&board, 0, 0));
        board.update_board(0, 0, &Stone::Black);
        board.update_board(5, 5, &Stone::Black);
        board.update_board(0, 8, &Stone::White);
        assert!(!is_empty_point(&board, 0, 0));
        assert!(!is_empty_point(&board, 5, 5));
        assert!(!is_empty_point(&board, 0, 8));
    }

    #[test]
    fn test_is_ko_fight() {
        let mut game = Game::new(Some((9, 9)));
        let moves_to_make = [
            ((0, 1), Stone::Black),
            ((1, 0), Stone::Black),
            ((1, 2), Stone::Black),
            ((2, 0), Stone::White),
            ((3, 1), Stone::White),
            ((2, 2), Stone::White),
            ((2, 1), Stone::Black),
            ((1, 1), Stone::White),
        ];

        // . B W . . . . . .
        // B . . W . . . . .
        // . B W . . . . . .

        for (point, stone_type) in moves_to_make.iter() {
            let _ = game.make_move(point.0, point.1, stone_type);
        }
        assert!(is_ko_fight(
            &game.board,
            game.get_last_board_state(),
            2,
            1,
            &Stone::Black
        ));
    }

    #[test]
    fn test_is_suicice_move() {
        let mut game = Game::new(Some((9, 9)));
        let moves_to_make = [((1, 0), Stone::Black), ((0, 1), Stone::Black)];

        // W B . . . . . . .
        // B . . . . . . . .

        for (point, stone_type) in moves_to_make.iter() {
            let _ = game.make_move(point.0, point.1, stone_type);
        }

        assert!(is_suicide_move(&game.board, 0, 0, &Stone::White));
    }

    #[test]
    fn test_is_valid_range() {
        let size = (9, 9);
        assert!(is_valid_range(&size, 0, 0));
        assert!(!is_valid_range(&size, 9, 9));
    }

    #[test]
    fn test_is_self_capture_move() {
        let mut game = Game::new(None);
        let moves_to_make = [
            ((8, 0), Stone::White),
            ((7, 0), Stone::Black),
            ((7, 1), Stone::Black),
            ((8, 2), Stone::Black),
        ];

        // . . . . . . . B W
        // . . . . . . . B .
        // . . . . . . . . B
        // . . . . . . . . .

        for (point, stone_type) in moves_to_make.iter() {
            let _ = game.make_move(point.0, point.1, stone_type);
        }
        assert!(is_self_capture_move(&game.board, 8, 1, &Stone::White))
    }
    #[test]
    fn test_is_valid_move() {
        let mut game = Game::new(Some((9, 9)));
        let moves_to_make = [
            ((0, 1), Stone::Black),
            ((1, 0), Stone::Black),
            ((1, 2), Stone::Black),
            ((2, 0), Stone::White),
            ((3, 1), Stone::White),
            ((2, 2), Stone::White),
            ((0, 3), Stone::Black),
            ((8, 0), Stone::White),
            ((7, 0), Stone::Black),
            ((7, 1), Stone::Black),
            ((8, 2), Stone::Black),
            ((2, 1), Stone::Black),
            ((1, 1), Stone::White),
        ];

        // . B W . . . . B W
        // B W . W . . . B .
        // . B W . . . . . B
        // B . . . . . . . .

        for (point, stone_type) in moves_to_make.iter() {
            let _ = game.make_move(point.0, point.1, stone_type);
        }
        game.display_board();
        assert_eq!(
            is_valid_move(
                &game.board,
                game.get_last_board_state(),
                2,
                1,
                &Stone::Black
            ),
            Err(MoveError::KoFight)
        );

        assert_eq!(
            is_valid_move(
                &game.board,
                game.get_last_board_state(),
                0,
                2,
                &Stone::White
            ),
            Err(MoveError::Suicide)
        );

        assert_eq!(
            is_valid_move(
                &game.board,
                game.get_last_board_state(),
                8,
                1,
                &Stone::White
            ),
            Err(MoveError::SelfCapture)
        );
        assert_eq!(
            is_valid_move(
                &game.board,
                game.get_last_board_state(),
                0,
                0,
                &Stone::White
            ),
            Ok(())
        );
    }
}

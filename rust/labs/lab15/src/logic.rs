use crate::board::{Board, BoardType};
use crate::enums::Stone;
use crate::rule::is_valid_move;
use std::collections::HashSet;

/// Gets the all valid moves based on the current board state
/// # Arguments
/// * `board` - The current board state
/// * `prev_board` - The previous board state
/// * `stone_type` - The type of stone to place
/// # Returns
/// * `Vec<(usize, usize)>` - Valid moves
pub fn get_valid_moves(curr_board: &Board, prev_board: &BoardType, stone_type: &Stone) -> Vec<(usize, usize)> {
    let mut valid_moves = Vec::new();

    for y in 0..curr_board.size.0 {
        for x in 0..curr_board.size.1 {
                if is_valid_move(curr_board, prev_board, x, y, stone_type).is_ok() {
                    valid_moves.push((x, y));
            }
        }
    }

    valid_moves
}


/// Gets the best move based on the current board state
/// # Arguments
/// * `board` - The current board state
/// * `prev_board` - The previous board state
/// * `stone_type` - The type of stone to place
/// # Returns
/// * `(usize, usize)` - The best move
pub fn get_best_move(curr_board: &Board, prev_board: &BoardType, stone_type: &Stone) -> (usize, usize) {
    let valid_moves: Vec<(usize, usize)> = get_valid_moves(curr_board, prev_board, stone_type);
    let mut best_move = (0, 0);
    let mut max_points = 0;

    for &(x, y) in &valid_moves {
        let mut board_copy = curr_board.clone();
        board_copy.set_stone(x, y, *stone_type);
        let points = board_copy.remove_dead_groups(Some((x, y)));

        if points > max_points {
            max_points = points;
            best_move = (x, y);
        }
    }

    // if no points can be scored, return the first valid move
    if best_move == (0, 0) {
        *valid_moves.first().unwrap_or(&(0, 0))
    }
    else {
         best_move
    }
}

/// Explores the board to find the liberties of a given group
/// # Arguments
/// * `board` - The current board state
/// * `x` - The x-coordinate
/// * `y` - The y-coordinate
/// # Returns
/// * `HashSet<(usize, usize)>` - The liberties of the given group
pub fn explore_neighbors_freedom(board: &Board, x: usize, y: usize) -> HashSet<(usize, usize)> {
    fn process_freedoms(
        self_board: &Board,
        x: usize,
        y: usize,
        visited: &mut HashSet<(usize, usize)>,
        liberties: &mut HashSet<(usize, usize)>,
        stone_type: &Stone,
    ) {
        if visited.contains(&(x, y)) || self_board.get_stone(x, y) != stone_type {
            return;
        }

        visited.insert((x, y));

        let neighbors = check_neighbors(&self_board.size, x, y);
        for &(nx, ny) in &neighbors {
            if nx < self_board.size.0 && ny < self_board.size.1 && !visited.contains(&(nx, ny)) {
                if self_board.get_stone(nx, ny) == stone_type {
                    process_freedoms(self_board, nx, ny, visited, liberties, stone_type);
                } else if self_board.get_stone(nx, ny) == &Stone::Empty {
                    liberties.insert((nx, ny));
                }
            }
        }
    }

    let mut visited: HashSet<(usize, usize)> = HashSet::new();
    let mut liberties: HashSet<(usize, usize)> = HashSet::new();
    process_freedoms(board, x, y, &mut visited, &mut liberties, board.get_stone(x, y));

    liberties
}


/// Count the number of groups on the board
/// # Arguments 
/// * `board` - Board state
/// # Returns
/// * `u16` - The number of groups
pub fn count_groups(board: &Board) -> u16 {
    fn dfs(
        x: usize,
        y: usize,
        stone_type: &Stone,
        checked_stones: &mut HashSet<(usize, usize)>,
        board: &Board,
    ) {
        if x >= board.size.0
            
                || y >= board.size.1
                || checked_stones.contains(&(x, y)) // already checked 
                || board.get_stone(x, y) != stone_type
        // not the same stone type
        {
            return;
        }
        checked_stones.insert((x, y));
        let neighbors =check_neighbors(&board.size, x, y);
        for &(x, y) in &neighbors {
            dfs(x, y, stone_type, checked_stones, board);
        }
    }

    let mut num_groups: u16 = 0;
    let mut checked_stones = HashSet::new();

    board.board.iter().for_each(|(&(x,y), &stone)| {
        if stone != Stone::Empty && !checked_stones.contains(&(x, y)) {
            dfs(x, y, &stone, &mut checked_stones, board);
            num_groups += 1;
        }
    });
    num_groups
}

/// Find the best move to extend liberties for a *given* group
/// # Arguments
/// * `board` - The current board state
/// * `x` - The x-coordinate
/// * `y` - The y-coordinate
/// # Returns
/// * `(usize, usize)` - The best move to extend liberties for the group
pub fn find_best_move_for_freedom(board: &Board, x: usize, y: usize) -> (usize, usize) {
    let liberties = explore_neighbors_freedom(board, x, y);
    let mut max_liberties = 0;
    let mut best_move = (0, 0);

    let mut board_copy = board.clone();

    for (nx, ny) in liberties {
        board_copy.update_board(nx, ny, board.get_stone(x, y));
        let new_liberties = explore_neighbors_freedom(&board_copy, nx, ny).len();
        if new_liberties > max_liberties {
            max_liberties = new_liberties;
            best_move = (nx, ny);
        }
        board_copy.update_board(nx, ny, &Stone::Empty);
    }

    best_move
}

/// Check the neighbors of a given point
/// # Arguments
/// * `size` - The size of the board
/// * `x` - The x-coordinate
/// * `y` - The y-coordinate
/// # Returns
/// * `Vec<(usize, usize)>` - The neighbors of the point
pub fn check_neighbors(size: &(usize, usize), x: usize, y: usize) -> Vec<(usize, usize)> {
    let mut neighbors: Vec<(usize, usize)> = vec![];

    // left
    if x > 0 {
        neighbors.push((x - 1, y));
    }
    // right
    if x < size.0 - 1 {
        neighbors.push((x + 1, y));
    }
    // up
    if y > 0 {
        neighbors.push((x, y - 1));
    }
    // down
    if y < size.1 - 1 {
        neighbors.push((x, y + 1));
    }
    neighbors
}


#[cfg(test)]
mod tests {
    use super::*;

    use crate::game::Game;

    #[test]
    fn test_get_best_mvoe() {
        let mut game = Game::new(Some((9, 9)));
        let moves_to_make = [
            ((0, 0), Stone::Black),
            ((1, 0), Stone::White),

            ((7,0), Stone::Black),
            ((7,1), Stone::Black),
            ((8,0), Stone::White),
            ((8,1), Stone::White)
        ];
        
        // B W . . . . . B W
        // . . . . . . . B W
        // . . . . . . . . .

        for (point, stone_type) in moves_to_make.iter() {
            let _ = game.make_move(point.0, point.1, stone_type);
        }

        let best_move = get_best_move(&game.board, game.get_last_board_state(), &Stone::White);
        assert_eq!(best_move, (0, 1));
        let best_move = get_best_move(&game.board, game.get_last_board_state(), &Stone::Black);
        assert_eq!(best_move, (8, 2));
    }

    #[test]
    fn test_find_best_move_for_freedom() {
        let mut game = Game::new(Some((9, 9)));

        let moves_to_make = [
            ((0, 1), Stone::Black),
            ((1, 0), Stone::Black),
            ((1, 2), Stone::Black),

            ((2, 0), Stone::White),
            ((3, 1), Stone::White),
            ((2, 2), Stone::White),

            ((2, 1), Stone::Black),
        ];

        // . B W . . . . . .
        // B . B W . . . . .
        // . B W . . . . . .
        // . . . . . . . . .
        // . . . . . . . . .

        for (point, stone_type) in moves_to_make.iter() {
            let _ = game.make_move(point.0, point.1, stone_type);
        }

        let best_move_free = find_best_move_for_freedom(&game.board, 0, 1);
        assert_eq!(best_move_free, (0, 2));

        let best_move_free = find_best_move_for_freedom(&game.board, 2, 0);
        assert_eq!(best_move_free, (3, 0));
    }

    #[test]
    fn test_check_neighbors() {
        let size = (9,9);

        let neighbors_1 = vec![(0, 1), (2, 1), (1, 0), (1, 2)];
        let neighbors_2 = vec![(1, 0), (0, 1)];

        assert_eq!(neighbors_1, check_neighbors(&size, 1, 1));
        assert_eq!(neighbors_2, check_neighbors(&size, 0,0))
    }

    #[test]
    fn test_count_groups() {
        let mut game = Game::new(Some((9, 9)));
        let moves_to_make = [
            ((1, 1), Stone::White),
            ((2, 1), Stone::White),
            ((1, 2), Stone::White),
            ((2, 2), Stone::White),

            ((3, 1), Stone::Black),
            ((4, 2), Stone::Black),
            ((5, 2), Stone::Black),
        ];
        // . B W . . . . . .
        // B . . W . . . . .
        // . B W . . . . . .
        for (point, stone_type) in moves_to_make.iter() {
            let _ = game.make_move( point.0, point.1, stone_type);
        }
        assert_eq!(count_groups(&game.board), 3);
    }

}

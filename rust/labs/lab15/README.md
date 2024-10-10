# Requirements

## Program for visualising and analysing Go game problems. The program should be able to

- [x] Represent the game state
- [x] Represent the stones on the board
- [x] Represent the groups

## Program can answer simple game state questions like

- [x] How many whit/black groups are on the board?
- [x] How many degrees of freedom a given group has?

## SGF specifics

- [x] Loading board state from SGF files
- [x] Saving board state to SGF files

## When playing the game, it should

- [x] update the game state (make moves)
- [x] detect illegal moves
- [x] properly capture stones

#### Computer specifics

- [x] Find a single white move that eliminates the most black stones (exhaustive search)
- [x] Find a single white move that extends the degrees of freedom of a given group the most

## User interface

- [x] Simple keyboard entry and terminal printouts are sufficient as UI. Use Text-based, single computer UI.

## Client-server mode

- [] (_Almost done_) Implement client-server game mode. The server hosts the match and at least two clients can connect to it.

## Other minor features

- [x] Score system

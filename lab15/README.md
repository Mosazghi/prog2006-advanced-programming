# Lab15: The game of GO

* Deadline: **no deadline, portfolio only**
* Use of ChatGPT and Co-pilot: **decide for yourself**
* Language: Haskell or Rust
* Difficulty: hard



# The task

* Program for visualising and analysing Go game problems. The program should be able to:
   * Represent the game state
   * Represent the stones on the board
   * Represent the groups
* Program can answer simple game state questions like:
   * How many white (black) groups are on the board?
   * How many degrees of freedom a given group has?
* Loading board state from [SGF](https://en.wikipedia.org/wiki/Smart_Game_Format) files.
* Ability to dump the board state as [SFG](https://en.wikipedia.org/wiki/Smart_Game_Format) sequence.
* Updating the board state (game moves).
* Ability to detect illegal game moves.
* Ability to properly capture stones.
* Updating the board state via computer-generated moves, for example:
   * Find a single white move that eliminates the most black stones (exhaustive search)
   * Find a single white move that extends the degrees of freedom of a given group the most


# User interface

* Simple keyboard entry and terminal printouts are sufficient as UI. Use Text-based, single computer UI.
* For more experienced students, GUI can be implemented.


# Ambitious students

* Write a functions that returns `True` if two board states are isomorphic. Use translation, rotation, and symmetry operations for isomorphism tests. We assume a stones and groups must maintain the exact freedom degree counts. Or, in other words, given two SGF files with a board state only, the program checks if the two games are the same (isomorphic for translation, rotation and symmetry).
* Write socket-based interface that your game can be run in a server mode, and it can be interacted with via a socket, such that two computer implementations, client and server, can play against each other.
* Make the computer to play with itself and implement some form of smart moves such that the computer goes beyond just generating random legal moves.
* Make the computer to `remember` and `learn` from its own mistakes. 


# Important

* The notes above ARE NOT a requirement specification.
* The specification is up to you. You need to write down your own specification for the lab yourself. Below are just the suggestions.
* If you opt for implementing the socket-based gameplay, follow the
commands specification below, in order to be able to be interoperable with other implementations.



# Client-server mode

To be able to play against other students (and Mariusz's) implementations, we need to have a standarised interaction protocol. The protocol consists of commands that can be issued as human readable Strings over a socket, in a `request-response` fashion.
This is similar to HTTP requests. Note, that `request-response` can be initiated both, from the server side, as well as from the client side.

* **Errors.** If there is a communication error, the response always starts with `ERR: ` followed by human readable error message. For signifying response with no errors, `OK` should be returned.
* **Initialisation.** Upon connection with the server, the client must issue `NEWGAME` command, to decide the size of the board, and if the client is WHITE or BLACK. The response is `OK`, or `ERR`.
* **SGF initialisation.** The `NEWGAME` starts a new game on an empty board. In order to enable handicap, resume previously stored game, or just initialise the board with a game problem, `NEWSGF` command can be used. This command will take SGF sequence as parameter and will initialise the stones on an empty board following the SGF sequence.
* **Moves.** The game proceeds by players making moves, starting with the BLACK first. For moves the `MOVE` command is used. If a player attempts to make a move out of sequence an error occurs.
* **PASS**. A player may decide not to place stones on the board, and issue `PASS` command.
* **Game state.** At any point of the game, command "BOARD" can be issued, and the receiving side will respond with a sequence representing the state of the board. 
The sequence will be in SGF format. See details below. This is mostly for debugging and sanity sake purposes when developing the system.
* **End of the game**. When two `PASS` commands are issued in a row, the game is considered `finished`. The program may try to automatically calculate the winner but this is not required. Humans can determine the final scoring. `BOARD` command can be issued to dump the final board state to SGF to help in scoring. New `NEWGAME` command can be issued by the client again to re-start new game again.
* **Saving the current state.** In order to save the current state of the game (or to save finished game), `SAVE` command can be used. The receiving side will save the current state of the game in a file. This can be used for debugging when playing with a computer over telnet and issuing commands by hand.


# Commands

* `NEWGAME <board size> <WHITE or BLACK>`
   - issued by the client side, initiates new game on a given board size with the client being WHITE or BLACK
* `NEWSGF <SGF sequence>`
   - issued by the client, resets the board to a given state given by the SGF sequence
* `MOVE <move>`
   - issued by the client or the server, makes a move. The move should follow SGF conventions.
* `PASS
* `BOARD`
   - asks the other party for their view of the current game board. The return is SGF sequence.
* `SAVE <filename>`
   - saving happens on the receiving side.


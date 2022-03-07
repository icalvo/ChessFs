# ♟ ChessFs

ChessFs is a chess engine developed in F#.

## StateActions pattern

The functional State pattern exposes all the possible operations (transition types) and only transitions to a different state if the operation is legal. This is a nice pattern if the number of transitions is low. However, when the number of legal operations and states is not well defined beforehand, it is better to only expose the legal operations.

The State Actions pattern is a way of doing this. For each state you will have a State-Actions object: it exposes representation data that allows UIs to show information about it, and a set of legal Actions. Each action has also representation data, but more importantly it has a function that return a new State-Actions object (which represents the result of executing said action on the former state).

This way, the UI can show just the actions that are legal at each step.

- StateActions:
	- StateRepresentation
	- Action list

- Action:
	- ActionRepresentation
	- unit -> StateActions

This is a really useful pattern with turn-based table games like chess or checkers because the state space is not bounded and the legal actions must be calculated for each state individually and can be quite a number of them. Also the number of possible actions (legal or not) is pretty big.

With this pattern you can easily implement a state machine. The input can be any kind of sequence; for example a sequence of strings with chess moves expressed in algebraic notation. The transition function will translate all the available Actions into the same a

## PlayerActionOutcome (StateActions)
This type has its representation and list of actions available through the functions `PlayerActionOutcome.representation` and `PlayerActionOutcome.actions`.

## ChessState (StateRepresentation)
A record with useful data to represent a chess board. The representation is hidden, but it is possible to access all the relevant information .
To build a `ChessState` there are three ways:
- `ChessState.initial`: this is a chess board in the initial position.
- `ChessState.fromFEN`: you can parse a FEN string to get a chess board. The parsing will return a `Result<ChessState>`. Since FEN strings do not incorporate the list of moves, it will return a state without moves or repeatable states, so the expected behaviour regarding three-fold repetition could be different from playing until that same position.
- `ChessState.fromRaw`: There is a record type called `RawChessState` which represents what you can find in a FEN string. Therefore it has the same limitations as `ChessState.fromFEN`, but it will not return parsing errors.
- `ChessState.stateAfter`: This will effectively run a series of commands in a state machine, starting with the initial position, and return the latest state. It also returns a `Result<ChessState>`. If it fails, it will indicate the parsing error or the engine error (e.g. an invalid move).

## ExecutableAction (Action)
The representation of the action is available with `ExecutableAction.representation` and the function that gives the next `PlayerActionOutcome` is `ExecutableAction.executefn`.

## PlayerActionDisplay (ActionRepresentation)
A discriminated union that represents the possible actions for a chess player.
# StateActions pattern

The functional State pattern exposes all the possible operations and only transitions to a different state if the operation is legal. This is a nice pattern if the number of operations is low. However, when the number of legal operations and states is not well defined beforehand, it is better to only expose the legal operations.

The State Actions pattern is a way of doing this. For each state you will have a State-Actions object: it exposes representation data that allows UIs to show information about it, and a set of legal Actions. Each action has also representation data, but more importantly it has a function that return a new State-Actions object (which represents the result of executing said action on the former state).

This way, the UI can show only that actions that are legal at each step.

- StateActions:
	- StateRepresentation
	- Action list

- Action:
	- ActionRepresentation
	- unit -> StateActions

This is a really useful pattern with turn-based table games like chess or checkers because the state space is not bounded and the legal actions must be calculated for each state individually and can be quite a number of them. Also the number of possible actions (legal or not) is pretty big.

With this pattern you can easily implement a state machine. The input can be any kind of sequence; for example a sequence of strings with chess moves expressed in algebraic notation. The transition function will translate all the available Actions into the same a

# PlayerActionOutcome (StateActions)

# DisplayInfo (StateRepresentation)

# ExecutableAction (Action)
Represents the possible actions of a player when it is his/her turn.

# PlayerActionDisplay (ActionRepresentation)
Represents the possible actions of a player when it is his/her turn.
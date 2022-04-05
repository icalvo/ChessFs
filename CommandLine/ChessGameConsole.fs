namespace ChessFs.CommandLine

module ChessGameConsole =

    open Output
    open GameConsole
    open ChessFs.Common.StateMachine
    open ChessFs.Chess
    open ChessFs.Chess.Engine
    open Notation

    let isFinish = function
        | Exiting -> true
        | _ -> false

    let printHelp () =
        printfn "You must input one of the actions in the list. These are the legal chess actions which are:
    :r --> Resign
    :a --> Accept a draw offer
    CHESSMOVE (e.g. Kd4) --> Play that move
    CHESSMOVE:d (e.g. g4:d) --> Play that move and offer a draw

    Once a move is played, the board will flip and you will be asked for an action for the opponent player.
    Also, you can enter 'h' to show this help and 'q' to quit the game."

    let chessConsoleTransition game =
        let actions = PlayerActionOutcome.actions
        let execute = ExecutableAction.executeFn
        let normalizeAction action = (executableActionToAlgebraic action).ToLowerInvariant()
        let normalizeInput (x: string) = x.ToLowerInvariant()
        gameConsoleTransition game actions execute normalizeAction normalizeInput printOutcome printHelp

    open StateMachine
    let chessConsoleStateMachine game input =
        let initialState = AskingAction game

        stateMachine (chessConsoleTransition game) isFinish initialState input
        |> Seq.toArray
module ChessGameConsole

open System
open Chess
open Notation
open Output
open GameConsole

let isFinish = function
    | Exiting -> true
    | _ -> false

let chessConsoleTransition2 game =
    let execute (x: ExecutableAction) = x.execute
    let normalizeAction action = (executableActionToAlgebraic action).ToLowerInvariant()
    gameConsoleTransition2 game actions execute normalizeAction printOutcome


let chessConsoleStateMachine game input =
    let initialState = AskingAction game

    StateMachine.stateMachine (chessConsoleTransition2 game) isFinish initialState input
    |> Seq.toArray
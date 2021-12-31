module ChessStateMachine

open StateActions
open Chess
open Notation

let chessStateMachineFrom initialState translateAction normalizeInput input =
    let actions = PlayerActionOutcome.actions
    let execute (x: ExecutableAction) = x.execute
    stateActionsFailingStateMachine initialState actions translateAction normalizeInput execute input

let chessStateMachine translateAction = chessStateMachineFrom newChessGame translateAction

let toLowerInvariant (s: string) = s.ToLowerInvariant()

let algebraicChessStateMachine translateInput input = chessStateMachine (executableActionToAlgebraic >> toLowerInvariant) (translateInput >> toLowerInvariant) input

let algebraicStringChessStateMachine input = algebraicChessStateMachine id input


let chessIgnoringStateMachineFrom initialState translateAction normalizeInput input =
    let actions = PlayerActionOutcome.actions
    let execute (x: ExecutableAction) = x.execute
    stateActionsIgnoringStateMachine initialState actions translateAction normalizeInput execute input

let chessIgnoringStateMachine translateAction = chessStateMachineFrom newChessGame translateAction

let algebraicChessIgnoringStateMachine translateInput input = chessStateMachine (executableActionToAlgebraic >> toLowerInvariant) (translateInput >> toLowerInvariant) input
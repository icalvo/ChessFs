module ChessStateMachine

open StateActions
open Chess
open Notation

let chessStateMachineFrom initialState translateAction normalizeInput input =
    let actions = PlayerActionOutcome.actions
    let execute = ExecutableAction.executefn
    stateActionsFailingStateMachine initialState actions translateAction normalizeInput execute input

let chessStateMachine translateAction = chessStateMachineFrom newStandardChessGame translateAction

let toLowerInvariant (s: string) = s.ToLowerInvariant()

let algebraicChessStateMachine translateInput input = chessStateMachine (executableActionToAlgebraic >> toLowerInvariant) (translateInput >> toLowerInvariant) input

let algebraicStringChessStateMachine input = algebraicChessStateMachine id input


let chessIgnoringStateMachineFrom initialState translateAction normalizeInput input =
    let actions = PlayerActionOutcome.actions
    let execute = ExecutableAction.executefn
    stateActionsIgnoringStateMachine initialState actions translateAction normalizeInput execute input

let chessIgnoringStateMachine translateAction = chessIgnoringStateMachineFrom newStandardChessGame translateAction

let algebraicChessIgnoringStateMachine translateInput input = chessIgnoringStateMachine (executableActionToAlgebraic >> toLowerInvariant) (translateInput >> toLowerInvariant) input

let algebraicStringChessIgnoringStateMachine input = algebraicChessIgnoringStateMachine id input

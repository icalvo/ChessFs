module ChessStateMachine

open StateActions
open Engine
open Notation

let chessStateMachineFrom initialState translateAction normalizeInput input =
    let actions = PlayerActionOutcome.actions
    let execute = ExecutableAction.executeFn
    stateActionsFailingStateMachine initialState actions translateAction normalizeInput execute input

let chessStateMachine translateAction = chessStateMachineFrom (initialStandardChessPosition()) translateAction

let toLowerInvariant (s: string) = s.ToLowerInvariant()

let algebraicChessStateMachine translateInput input = chessStateMachine (executableActionToAlgebraic >> toLowerInvariant) (translateInput >> toLowerInvariant) input

let algebraicStringChessStateMachine input = algebraicChessStateMachine id input

let algebraicStringChessActionsTransitionIgnoring =
    actionsTransitionIgnoring PlayerActionOutcome.actions (executableActionToAlgebraic >> toLowerInvariant) toLowerInvariant ExecutableAction.executeFn

let chessIgnoringStateMachineFrom initialState normalizeAction normalizeInput input =
    let actions = PlayerActionOutcome.actions
    let execute = ExecutableAction.executeFn
    stateActionsIgnoringStateMachine initialState actions normalizeAction normalizeInput execute input

let chessIgnoringStateMachine translateAction = chessIgnoringStateMachineFrom (initialStandardChessPosition()) translateAction

let algebraicChessIgnoringStateMachine translateInput input = chessIgnoringStateMachine (executableActionToAlgebraic >> toLowerInvariant) (translateInput >> toLowerInvariant) input

let algebraicStringChessIgnoringStateMachine input = algebraicChessIgnoringStateMachine id input

let chessFailingStateMachineFrom initialState normalizeAction normalizeInput input =
    let actions = PlayerActionOutcome.actions
    let execute = ExecutableAction.executeFn
    stateActionsFailingStateMachine initialState actions normalizeAction normalizeInput execute input

let chessFailingStateMachine translateAction = chessFailingStateMachineFrom (initialStandardChessPosition()) translateAction

let algebraicChessFailingStateMachine translateInput input = chessFailingStateMachine (executableActionToAlgebraic >> toLowerInvariant) (translateInput >> toLowerInvariant) input

let algebraicStringChessFailingStateMachine input = algebraicChessFailingStateMachine id input

module ChessState =
    let stateAfter: (string seq -> ChessState) =
        algebraicStringChessIgnoringStateMachine
        >> Seq.last
        >> PlayerActionOutcome.state

    let stateAfterFailing: (string seq -> Result<ChessState, string>) =
        algebraicStringChessFailingStateMachine
        >> Seq.last
        >> Result.map PlayerActionOutcome.state

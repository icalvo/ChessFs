namespace ChessFs.Tests

module Helpers =
    open ChessFs.Chess
    open Notation

    let availableActions = function
        | Ok x ->
            match x with
            | GameStarted (_, availableActions)
            | DrawOffered (_, availableActions)
            | PlayerMoved (_, availableActions) ->
                List.map executableActionToAlgebraic availableActions
            | _ -> List.empty
        | Error msg -> [$"Invalid board: %A{msg}"]


namespace ChessFs.Api.Db

open System.Collections.Generic
open Engine
open Notation
open ChessStateMachine
open ChessFs.Api.Payloads
open YoLo.Result.Operators

module GameRepository =
    let private gameStorage = new Dictionary<int, PlayerActionOutcome>()

    let createGame () =
        let id = gameStorage.Values.Count + 1
        let newGame = initialStandardChessPosition()
        gameStorage.Add(id, newGame)
        let actions = newGame |> PlayerActionOutcome.actions |> List.map executableActionToAlgebraic
        { Id = id; State = PlayerActionOutcome.state newGame; Actions = actions }

    let getGame gameId =
        if gameStorage.ContainsKey(gameId) then
            Ok { Id = gameId; State = PlayerActionOutcome.state gameStorage[gameId]; Actions = [] }
        else
            Error $"Game {gameId} not found"

    let play gameId move =
        let transition input pos = algebraicStringChessActionsTransitionIgnoring pos input
        let actions = PlayerActionOutcome.actions >> List.map executableActionToAlgebraic
        getGame gameId
        >!> fun x -> x.State
        >>= setupStandardChessPosition
        >!> transition move
        >>* fun x -> gameStorage[gameId] <- x
        >!> fun outcome -> { Id = gameId; State = PlayerActionOutcome.state outcome; Actions = actions outcome }

    let deletePerson personId =
        gameStorage.Remove personId |> ignore

    let itExists personId =
        gameStorage.ContainsKey(personId)

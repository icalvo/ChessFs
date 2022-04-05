namespace ChessFs.Api.Db

open ChessFs.Chess

module GameRepository =
    open System.Collections.Generic
    open ChessFs.Chess.Engine
    open Notation
    open StateMachine
    open Setup
    open ChessFs.Api.Payloads
    open ChessFs.Api.Rest
    open YoLo.Result.Operators

    let private gameStorage = Dictionary<int, PlayerActionOutcome>()

    let createGame () =
        let id = gameStorage.Values.Count + 1
        let newGame = initialStandardChessPosition()
        gameStorage.Add(id, newGame)
        let actions = newGame |> PlayerActionOutcome.actions |> List.map executableActionToAlgebraic
        { Id = id; State = PlayerActionOutcome.state newGame; Actions = actions }

    let getGame: int -> Result<ChessPayload, DatabaseError> = fun gameId ->
        if gameStorage.ContainsKey(gameId) then
            Ok { Id = gameId; State = PlayerActionOutcome.state gameStorage[gameId]; Actions = [] }
        else
            Error (NotFound gameId)
    
    let play: int -> string -> Result<ChessPayload, GameRepositoryError> = fun gameId move ->
        let transition input pos = algebraicStringChessActionsTransitionIgnoring pos input
        let actions = PlayerActionOutcome.actions >> List.map executableActionToAlgebraic
        getGame gameId
        >!> fun x -> x.State
        |> Result.mapError Database
        >>= (setupStandardChessPosition >> Result.mapError Validation)
        >!> transition move
        >>* fun x -> gameStorage[gameId] <- x
        >!> fun outcome -> { Id = gameId; State = PlayerActionOutcome.state outcome; Actions = actions outcome }

    let deletePerson personId =
        gameStorage.Remove personId |> ignore

    let itExists personId =
        gameStorage.ContainsKey(personId)

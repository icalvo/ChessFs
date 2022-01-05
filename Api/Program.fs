open System
open System.Threading

open FSharp.Json
open FParsec
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Chess
open ChessStateMachine
open ChessParsing
open YoLo.Result.Operators


module Result =
    let toValue fnOk fnError = function | Result.Ok x -> fnOk x | Result.Error x -> fnError x

[<EntryPoint>]
let main argv = 
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }

    let rawBody (req: HttpRequest) = req.rawForm
    let toUTF8 (str: byte []) = System.Text.Encoding.UTF8.GetString(str)

    let body = rawBody >> toUTF8

    let transition input pos = algebraicStringChessActionsTransitionIgnoring pos input

    let (>>>) f1 (fnOk, fnErr) = f1 |> Result.toValue fnOk fnErr

    let aa (fen, move) =
        let stateRep =
            match parseFEN fen with
            | Success (r, _, _) -> Result.Ok r
            | Failure (f, _, _) -> Result.Error f

        stateRep
        >>= setupStandardChessPosition
        >!> (transition move)
        >!> Json.serialize
        |> Result.toValue OK BAD_REQUEST

    let app = choose [
            GET >=> path "/chess" >=> OK (Json.serialize (representation initialGameState))
            GET >=> pathScan "/chess/%s/%s" aa
        ]

    let listening, server = startWebServerAsync conf app
    
    Async.Start(server, cts.Token)
    printfn "Make requests now"
    Console.ReadKey true |> ignore
    
    cts.Cancel()

    0 // return an integer exit code
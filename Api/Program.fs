namespace ChessFs.Api

open Suave.Web
open ChessFs.Api.Db
open ChessFs.Api.Rest

module Main =
    [<EntryPoint>]
    let main argv =
        let chessWebPart = rest "chess" {
            Create = GameRepository.createGame
            Play = GameRepository.play
            GetById = GameRepository.getGame
            ItExists = GameRepository.itExists
        }
        startWebServer defaultConfig chessWebPart
        0 // return an integer exit code

//[<EntryPoint>]
//let main argv = 
//    let cts = new CancellationTokenSource()
//    let conf = { defaultConfig with cancellationToken = cts.Token }

//    let rawBody (req: HttpRequest) = req.rawForm
//    let toUTF8 (str: byte []) = System.Text.Encoding.UTF8.GetString(str)

//    let body = rawBody >> toUTF8

//    let transition input pos = algebraicStringChessActionsTransitionIgnoring pos input

//    let (>>>) f1 (fnOk, fnErr) = f1 |> Result.toValue fnOk fnErr

//    let FENofUrl = String.replace "|" "/" >> String.replace "_" " "
//    let FENtoUrl = String.replace "/" "|" >> String.replace " " "_"

//    let cc (gameId: string) move request =
//        let stateRep =
//            match gameId |> (fun x -> Guid.Parse(x)) |> GameRepository.get with
//            | Some state -> Result.Ok state
//            | None -> Result.Error $"Game {id} is not found"

//        let output outcome =
//            let fen =
//                outcome
//                |> PlayerActionOutcome.state
//                |> ChessState.toFEN
//                |> FENtoUrl

//            let moves =
//                outcome
//                |> PlayerActionOutcome.actions
//                |> List.map executableActionToAlgebraic
//                |> List.map (fun move -> $"http://{request.rawHost}/chess/{fen}/{move}")

//            { fen = $"http://{request.rawHost}/chess/{fen}"; moves = moves }

//        let tran = match move with | Some m -> transition m | None -> id
//        stateRep
//        >>= setupStandardChessPosition
//        >!> tran
//        >!> output
//        >!> Json.serialize
//        |> Result.toValue OK BAD_REQUEST

//    let aa (fen, move) (request: HttpRequest) =
//        cc fen (Some move) request

//    let bb gameId (request: HttpRequest) =
//        cc gameId None request

//    let createGame() =
//        let gameId = GameRepository.create.ToString()

//        OK ("{ \"id\": \"" + gameId + "\" }")

//    let app =
//        choose [
//            POST >=> path "/chess/" >=> createGame()
//            GET >=> pathScan "/chess/%s/%s" (aa >> request)
//            GET >=> pathScan "/chess/%s/fen" (bb >> request)
//        ]
//        >=> setHeader "Content-Type" "application/json"

//    let listening, server = startWebServerAsync conf app
    
//    Async.Start(server, cts.Token)
//    printfn "Make requests now"
//    Console.ReadKey true |> ignore
    
//    cts.Cancel()

//    0 // return an integer exit code
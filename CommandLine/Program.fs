open ChessFs.Common
open ChessFs.Chess
open ChessFs.Chess.Engine
open Notation
open StateMachine
open ChessFs.CommandLine.GameConsole
open ChessFs.CommandLine.ChessGameConsole
open Setup

type State =
    | State

let finalPGN: string list -> string =
    algebraicStringChessStateMachine
    >> Seq.last
    >> Result.map PlayerActionOutcome.toCommentedPGN
    >> Result.mapError (fun x -> "Failure: " + x)
    >> Result.toValue

[<EntryPoint>]
let main argv =
    printfn "Welcome to ChessFs! This console application shows a fully functional chess engine."
    printfn "All you see in each ply is provided by the engine: the board state, the list of possible moves and the outcome of the former move."

    let game = initialStandardChessPosition()
    
    let commandLineInput = argv |> Seq.map (fun x -> lazy x);
    let input = commandLineInput |> Seq.append consoleInput

    chessConsoleStateMachine game input |> ignore

    printfn "Bye!"
    0
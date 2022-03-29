open System
open Engine
open Utils
open Notation
open ChessStateMachine
open GameConsole
open ChessGameConsole

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
    
//    let actions = [ "e4"; "e5"; "Bc4"; "Nc6"; "Qh5"; "Nf6"; "Qxf7" ] |> finalPGN
//    let x = Console.ReadLine()
    
    let commandLineInput = argv |> Seq.map (fun x -> lazy x);
    let input = commandLineInput |> Seq.append consoleInput

    chessConsoleStateMachine game input |> ignore

    printfn "Bye!"
    0
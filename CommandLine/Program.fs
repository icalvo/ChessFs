open Chess
open Output
open GameConsole
open ChessGameConsole

[<EntryPoint>]
let main argv =
    printfn "Welcome to ChessFs! This console application shows a fully functional chess engine."
    printfn "All you see in each ply is provided by the engine: the board state, the list of possible moves and the outcome of the former move."

    let game = newChessGame
    printOutcome game
    let commandLineInput = argv |> Seq.map (fun x -> lazy x);
    let input = commandLineInput |> Seq.append consoleInput

    chessConsoleStateMachine game input |> ignore

    printfn "Bye!"
    0
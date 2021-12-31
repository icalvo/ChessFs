module Output

open System
open CoreTypes
open Chess
open Microsoft.FSharp.Core.Printf
open Utils
open Notation

// CONSOLE OUTPUT

let cprintf fc bc fmt = 
    kprintf
        (fun s ->
            let oldfc = Console.ForegroundColor
            let oldbc = Console.BackgroundColor
            try
                Console.ForegroundColor <- fc;
                Console.BackgroundColor <- bc;
                Console.Write s
            finally
                Console.ForegroundColor <- oldfc;
                Console.BackgroundColor <- oldbc)
        fmt

let cprintfn fc bc fmt =
    cprintf fc bc fmt
    printfn ""

let cellBackground (file: File, rank: Rank) =
    match (file.toInt + rank.toInt) % 2 with
    | 0 -> ConsoleColor.Green
    | 1 -> ConsoleColor.DarkGreen
    | _ -> failwith "Cannot happen"

let cellForeground =
    function
    | PieceSquare (Piece (White, _), _) -> ConsoleColor.White
    | PieceSquare (Piece (Black, _), _) -> ConsoleColor.Black
    | EmptySquare _ -> ConsoleColor.Black

let printSquare sq =
    cprintf (cellForeground sq) (cellBackground (Square.piecePosition sq)) $"%s{squareToString sq}"

let printBoard (b: Square[,]) color =
    let colorFunc =
        match color with
        | White -> id
        | Black -> Array.rev

    let colorFunc2 =
        match color with
        | White -> id
        | Black -> Array.rev

    let indexesToIterate = colorFunc [|0..7|]

    let iterRow fn row =
        indexesToIterate
        |> Array.map (fun i -> b.[row, i])
        |> Array.iter fn

    let printSquareRow i =
        printf $"%i{8-i}"
        iterRow printSquare i
        printfn $"%i{8-i}"

    let filesHeader =
        (colorFunc2 [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"|])
        |> Array.map (fun x -> x.PadLeft(2, ' '))
        |> String.concat ""

    printfn $"%s{filesHeader}"
    indexesToIterate |> Array.iter printSquareRow
    printfn $"%s{filesHeader}"

let csl toString seq = seq |> Seq.map toString |> String.concat ", "

let actionsOutput =
    Seq.map executableActionToAlgebraic 
    >> String.concat ", "

let printActions = 
    actionsOutput
    >> printfn "%s"

let printMoves =
    movesToPGN >> printfn "Moves: %s"
 
let printOutcome (outcome: PlayerActionOutcome) =
    let repr = PlayerActionOutcome.representation outcome
    printBoard repr.board repr.playerInTurn
    outcome |> outcomeToSimplePGN |> printfn "%s"
    if repr.isCheck then
        printfn "CHECK!"
    printfn $"%A{repr.playerInTurn} to move"
    printfn ""
    match outcome with
    | Draw (_, _, drawType) -> 
        printfn $"GAME OVER - Draw by %A{drawType}"
        printfn ""
    | LostByResignation (_, player) -> 
        printfn $"GAME WON because %A{opponent player} resigned"
        printfn ""
    | WonByCheckmate (_, player) -> 
        printfn $"GAME WON by checkmating %A{player}"
        printfn ""
    | PlayerMoved (_, availableActions) ->
        printActions availableActions
    | GameStarted (_, availableActions) -> 
        printfn "GAME STARTED"
        printActions availableActions
    | DrawOffer (_, player, availableActions) ->
        printfn $"DRAW OFFERED by %A{player}"
        printActions availableActions
    | DrawDeclinement (_, player, availableActions) ->
        printActions availableActions
        printfn $"DRAW DECLINED by %A{player}"
 
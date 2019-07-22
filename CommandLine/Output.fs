module Output

open System
open CoreTypes
open Chess
open Microsoft.FSharp.Core.Printf

// STRING REPRESENTATIONS

type Shape
with
    member this.toString = 
        match this with
        | Pawn   -> "P"
        | Knight -> "N"
        | Bishop -> "B"
        | Rook   -> "R"
        | Queen  -> "Q"
        | King   -> "K"

type Rank
with
    member this.toString =
        match this with
        | R1 -> "1"
        | R2 -> "2"
        | R3 -> "3"
        | R4 -> "4"
        | R5 -> "5"
        | R6 -> "6"
        | R7 -> "7"
        | R8 -> "8"

type File
with
    member this.toAlgebraic =
        match this with
        | A -> "a"
        | B -> "b"
        | C -> "c"
        | D -> "d"
        | E -> "e"
        | F -> "f"
        | G -> "g"
        | H -> "h"
    member this.toNumeric =
        match this with
        | A -> "1"
        | B -> "2"
        | C -> "3"
        | D -> "4"
        | E -> "5"
        | F -> "6"
        | G -> "7"
        | H -> "8"

type Color with
    member this.toString =
        match this with
        | Black -> "b"
        | White -> "w"

let positionToAlgebraic ((f, r):Position) =
    sprintf "%s%s" f.toAlgebraic r.toString

let squareToString = function
    | EmptySquare _ -> "  "
    | PieceSquare (Piece (color, shape), _) -> sprintf "%s%s" color.toString shape.toString

let squareWithActions capList square =
    let capabilityAt pos = capList |> Seq.tryFind (fun (_, p) -> p = pos)

    let actionToString = function
        | Move _ -> "m"
        | Capture _ -> "x"
        | CaptureEnPassant _ -> "p"
        | CastleKingSide _ -> "k"
        | CastleQueenSide _ -> "q"
        | Promote _ -> "M"
        | CaptureAndPromote _ -> "C"

    let squareActionToString =
        square
        |> Square.position
        |> capabilityAt
        |> Option.map fst
        |> Option.map actionToString
        |> defaultArg <| " "

    (squareToString square) + squareActionToString

let plyToAlgebraic ply =
    let (sourcePos, targetPos) = Ply.positions ply
    let targetPosString = positionToAlgebraic targetPos
    let sourceFile = (fst sourcePos)
    match ply with
    | Move (Piece (_, Pawn), _, _) ->
        sprintf "%s" targetPosString
    | Move (Piece (_, shape), _, _) ->
        sprintf "%s%s" shape.toString targetPosString
    | Capture (Piece (_, shape), _, _) ->
        match shape with
        | Pawn -> sprintf "%sx%s" sourceFile.toAlgebraic targetPosString
        | _ -> sprintf "%sx%s" shape.toString targetPosString
    | CaptureEnPassant _ -> sprintf "%sx%s" sourceFile.toAlgebraic targetPosString
    | CastleKingSide _ -> "O-O"
    | CastleQueenSide _ -> "O-O-O"
    | Promote (_, _, _, tshape) -> sprintf "%s=%s" targetPosString tshape.toString
    | CaptureAndPromote (_, _, _, tshape) -> sprintf "%sx%s=%s" sourceFile.toAlgebraic targetPosString tshape.toString

let playerActionToAlgebraic = function
    | MovePiece ply -> plyToAlgebraic ply
    | Resign -> ":r"
    | OfferDraw -> ":d"

let printPositions =
    List.map positionToAlgebraic >> List.iter (printf "%A")

let cellColor (file: File, rank: Rank) =
    match (file.toInt + rank.toInt) % 2 with
    | 0 -> White
    | 1 -> Black
    | _ -> failwith "Cannot happen"

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
    cprintf (cellForeground sq) (cellBackground (Square.position sq)) "%s" (squareToString sq)

let printBoard2 (b: Square[,]) color =
    let iterRow fn row =
        [|0..7|]
        |> Array.map (fun i -> b.[row, i])
        |> Array.iter fn

    let printSquareRow i =
        printf "%i" (8-i)
        iterRow printSquare i
        printfn "%i" (8-i)

    let filesHeader =
        [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"|]
        |> Array.map (fun x -> x.PadLeft(2, ' '))
        |> String.concat ""

    printfn "%s" filesHeader
    [|0..7|] |> Array.iter printSquareRow
    printfn "%s" filesHeader

let csl toString seq = seq |> Seq.map toString |> String.concat ", "

let actionsOutput seq = 
    csl (fun moveInfo -> playerActionToAlgebraic moveInfo.action) seq


let printActions = 
    csl (fun moveInfo -> playerActionToAlgebraic moveInfo.action)
    >> printfn "%s"
 
let printDisplayInfo displayInfo =
    printBoard2 displayInfo.board displayInfo.playerToMove
    if displayInfo.canCastleKingside then
        printfn "Can castle kingside"
    if displayInfo.canCastleQueenside then
        printfn "Can castle queenside"
    if displayInfo.isCheck && Option.isSome displayInfo.playerToMove then
        printfn "CHECK!"
    displayInfo.playerToMove
    |> Option.map (fun p -> printfn "%A moves" p) |> ignore
    printfn "" 

let printOutcome = function
    | Draw (displayInfo, _) -> 
        displayInfo |> printDisplayInfo
        printfn "GAME OVER - Draw"
        printfn ""
    | LostByResignation (displayInfo, player) -> 
        displayInfo |> printDisplayInfo
        printfn "GAME WON because %A resigned" (opponent player)
        printfn ""
    | WonByCheckmate (displayInfo, player) -> 
        displayInfo |> printDisplayInfo
        printfn "GAME WON by checkmating %A" player
        printfn ""
    | PlayerMoved (displayInfo, availableActions) -> 
        displayInfo |> printDisplayInfo
        printActions availableActions
    | DrawOffer (_, player, _) ->
        printfn "DRAW OFFERED by %A" player
 
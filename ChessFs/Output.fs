module Output

open System
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
    
let board toString displayInfo =
    let stringMatrix = displayInfo |> Array2D.map toString

    [|0..7|]
    |> Array.map (fun i -> stringMatrix.[i, *])
    |> Array.map (String.concat "|")

let squareWithActions capList square =
    let capabilityAt pos = capList |> Seq.tryFind (fun (_, p) -> p = pos)

    let actionToString = function
        | Move -> "m"
        | Capture -> "x"
        | EnPassant -> "p"
        | CastleKingSide -> "k"
        | CastleQueenSide -> "q"
        | Promote -> "M"
        | CaptureAndPromote -> "C"

    let squareActionToString =
        square
        |> position
        |> capabilityAt
        |> Option.map fst
        |> Option.map actionToString
        |> defaultArg <| " "

    (squareToString square) + squareActionToString

let plyToAlgebraic ply =
    let (sourcePos, targetPos) = plyPositions ply
    let targetPosString = positionToAlgebraic targetPos
    let sourceFile = (fst sourcePos)
    match ply with
    | Ply.Move (Piece (_, Pawn), _, _) ->
        sprintf "%s" targetPosString
    | Ply.Move (Piece (_, shape), _, _) ->
        sprintf "%s%s" shape.toString targetPosString
    | Ply.Capture (Piece (_, shape), _, _) ->
        match shape with
        | Pawn -> sprintf "%sx%s" sourceFile.toAlgebraic targetPosString
        | _ -> sprintf "%sx%s" shape.toString targetPosString
    | Ply.EnPassant _ -> sprintf "%sx%s" sourceFile.toAlgebraic targetPosString
    | Ply.CastleKingSide _ -> "O-O"
    | Ply.CastleQueenSide _ -> "O-O-O"
    | Ply.Promote (_, _, _, tshape) -> sprintf "%s=%s" targetPosString tshape.toString
    | Ply.CaptureAndPromote (_, _, _, tshape) -> sprintf "%sx%s=%s" sourceFile.toAlgebraic targetPosString tshape.toString

let playerActionToAlgebraic = function
    | MovePiece ply -> plyToAlgebraic ply
    | Resign -> ":r"
    | OfferDraw -> ":d"

// CONSOLE OUTPUT

let cprintf c fmt = 
    kprintf
        (fun s ->
            let old = Console.ForegroundColor
            try
              Console.ForegroundColor <- c;
              Console.Write s
            finally
              Console.ForegroundColor <- old)
        fmt

let cprintfn c fmt =
    cprintf c fmt
    printfn ""

let printPositions =
    List.map positionToAlgebraic >> List.iter (printf "%A")

let printBoard b = 
    let printRow i x = printfn "%i|%s|%i" (8-i) x (8-i)
    
    let filesHeader =
        [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"|]
        |> Array.map (fun x -> x.PadLeft(3, ' '))
        |> String.concat ""

    printfn "%s" filesHeader
    b |> Array.iteri printRow
    printfn "%s" filesHeader

let csl toString seq = seq |> Seq.map toString |> String.concat ", "

let actionsOutput seq = 
    csl (fun moveInfo -> playerActionToAlgebraic moveInfo.action) seq


let printActions = 
    csl (fun moveInfo -> playerActionToAlgebraic moveInfo.action)
    >> printfn "%s"
 
let printDisplayInfo displayInfo = 
    board squareToString displayInfo.board |> printBoard
    if displayInfo.isCheck then printfn "CHECK!" else printfn "NOCHECK!"
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
 
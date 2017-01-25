module Output

open Domain

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
    member this.toString =
        match this with
        | A -> "a"
        | B -> "b"
        | C -> "c"
        | D -> "d"
        | E -> "e"
        | F -> "f"
        | G -> "g"
        | H -> "h"

type Color with
    member this.toString =
        match this with
        | Black -> "b"
        | White -> "w"

let positionToString ((f, r):Position) =
    sprintf "%s%s" f.toString r.toString

let printPositions =
    List.map positionToString >> List.iter (printf "%A")

let squareToString = function
    | EmptySquare _ -> "  "
    | PieceSquare (Piece (color, shape), _) -> sprintf "%s%s" color.toString shape.toString

let board toString displayInfo =
    let stringMatrix = displayInfo |> Array2D.map toString

    [|0..7|]
    |> Array.map (fun i -> stringMatrix.[i, *])
    |> Array.map (String.concat "|")

let printBoard b = 
    let printRow i x = printfn "%i|%s|%i" (8-i) x (8-i)
    
    let filesHeader =
        [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"|]
        |> Array.map (fun x -> x.PadLeft(3, ' '))
        |> String.concat ""

    printfn "%s" filesHeader
    b |> Array.iteri printRow
    printfn "%s" filesHeader

let squareWithActions capList square =
    let capabilityAt pos = capList |> Seq.tryFind (fun (_, p) -> p = pos)

    let actionToString = function
        | Move -> "m"
        | Capture -> "x"
        | EnPassant -> "p"
        | CastleKingSide -> "k"
        | CastleQueenSide -> "q"

    let squareActionToString =
        square
        |> position
        |> capabilityAt
        |> Option.map fst
        |> Option.map actionToString
        |> defaultArg <| " "

    (squareToString square) + squareActionToString

let playerActionToString = function
    | MovePiece (Piece (_, shape), sourcePos, action, targetPos) ->
        let targetPosString = positionToString targetPos
        let sourceFile = (fst sourcePos)
        match action with
        | Move ->
            match shape with
            | Pawn -> sprintf "%s" targetPosString
            | _ -> sprintf "%s%s" shape.toString targetPosString
        | Capture ->
            match shape with
            | Pawn ->
                
                sprintf "%sx%s" sourceFile.toString targetPosString
            | _ -> sprintf "%sx%s" shape.toString targetPosString
        | EnPassant -> sprintf "%sx%s" sourceFile.toString targetPosString
        | CastleKingSide -> "O-O"
        | CastleQueenSide -> "O-O-O"
    | Abandon -> "Abandon"

let printActions domainActions = 
    domainActions
    |> Seq.map (fun moveInfo -> playerActionToString moveInfo.action)
    |> String.concat ", "
    |> printfn "%s"
 
let printDisplayInfo displayInfo = 
    board squareToString displayInfo.board |> printBoard
    printfn "" 

let printActionResult formerPlayerActionResult =
    // handle each case of the result
    match formerPlayerActionResult with
    | Draw (displayInfo, _) -> 
        displayInfo |> printDisplayInfo
        printfn "GAME OVER - Draw"
        printfn ""
    | WonByAbandon (displayInfo, player) -> 
        displayInfo |> printDisplayInfo
        printfn "GAME WON because %A abandoned" (opponent player)
        printfn ""
    | WonByCheckMate (displayInfo, player) -> 
        displayInfo |> printDisplayInfo
        printfn "GAME WON by %A's checkmate" player
        printfn ""
    | PlayerWhiteToMove (displayInfo, availableActions) -> 
        displayInfo |> printDisplayInfo
        printfn "White to move" 
        printActions availableActions
    | PlayerBlackToMove (displayInfo, availableActions) -> 
        displayInfo |> printDisplayInfo
        printfn "Black to move" 
        printActions availableActions
 
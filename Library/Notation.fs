module Notation

open CoreTypes
open Chess
open Utils

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

type Piece with
    member this.toFEN =
        match this with
        | Piece (White, shape) -> shape.toString
        | Piece (Black, shape) -> shape.toString.ToLowerInvariant()

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

let plyOutputSuffix = function
    | RegularPly _ -> ""
    | CheckPly _ -> "+"
    | CheckmatePly _ -> "#"

let plyToAlgebraic plyOutput =
    let suffix = plyOutputSuffix plyOutput
    let ply = plyOutput.ply
    let (sourcePos, targetPos) = Ply.positions ply
    let targetPosString = positionToAlgebraic targetPos
    let sourceFile = (fst sourcePos)
    match ply with
    | Move (Piece (_, Pawn), _, _) ->
        sprintf "%s%s" targetPosString suffix
    | Move (Piece (_, shape), _, _) ->
        sprintf "%s%s%s" shape.toString targetPosString suffix
    | Capture (Piece (_, shape), _, _) ->
        match shape with
        | Pawn -> sprintf "%sx%s%s" sourceFile.toAlgebraic targetPosString suffix
        | _ -> sprintf "%sx%s" shape.toString targetPosString
    | CaptureEnPassant _ -> sprintf "%sx%s%s" sourceFile.toAlgebraic targetPosString suffix
    | CastleKingSide _ -> sprintf "O-O%s" suffix
    | CastleQueenSide _ -> sprintf "O-O-O%s" suffix
    | Promote (_, _, _, tshape) -> sprintf "%s=%s%s" targetPosString tshape.toString suffix
    | CaptureAndPromote (_, _, _, tshape) -> sprintf "%sx%s=%s%s" sourceFile.toAlgebraic targetPosString tshape.toString suffix

let playerActionToAlgebraic = function
    | MovePiece ply -> plyToAlgebraic (RegularPly ply)
    | Resign -> ":r"
    | OfferDraw _ -> ":d"
    | AcceptDraw -> ":a"
    | DeclineDraw _ -> ":d"

let printPositions =
    List.map positionToAlgebraic >> List.iter (printf "%A")

let cellColor (file: File, rank: Rank) =
    match (file.toInt + rank.toInt) % 2 with
    | 0 -> White
    | 1 -> Black
    | _ -> failwith "Cannot happen"

let movesToPGN =
    Seq.batch 2
    >> Seq.indexed
    >> Seq.map (fun (idx, plies) ->
        match plies with
        |[whitePly] -> sprintf "%i. %s" (idx+1) (plyToAlgebraic whitePly)
        |[whitePly; blackPly] -> sprintf "%i. %s %s" (idx+1) (plyToAlgebraic whitePly) (plyToAlgebraic blackPly)
        |_ -> "")
    >> String.concat " "

let outcomeToSimplePGN = function
| Draw (displayInfo, _, _) ->
    sprintf "%s 1/2-1/2" (movesToPGN displayInfo.moves)
| DrawDeclinement (displayInfo, _, _) ->
    sprintf "%s" (movesToPGN displayInfo.moves)
| LostByResignation (displayInfo, player) ->
    match player with
    | Black -> sprintf "%s 1-0" (movesToPGN displayInfo.moves)
    | White -> sprintf "%s 0-1" (movesToPGN displayInfo.moves)
| WonByCheckmate (displayInfo, player) -> 
    match player with
    | Black -> sprintf "%s# 1-0" (movesToPGN displayInfo.moves)
    | White -> sprintf "%s# 0-1" (movesToPGN displayInfo.moves)
| GameStarted _ ->
    "Game started!"
| PlayerMoved (displayInfo, _) ->
    movesToPGN displayInfo.moves
| DrawOffer (displayInfo, _, _) ->
    sprintf "%s" (movesToPGN displayInfo.moves)

let outcomeToPGN = function
| Draw (displayInfo, _, drawType) ->
    sprintf "%s 1/2-1/2 {%A}" (movesToPGN displayInfo.moves) drawType
| DrawDeclinement (displayInfo, _, _) ->
    sprintf "%s {Draw declined}" (movesToPGN displayInfo.moves)
| LostByResignation (displayInfo, player) ->
    match player with
    | Black -> sprintf "%s 1-0" (movesToPGN displayInfo.moves)
    | White -> sprintf "%s 0-1" (movesToPGN displayInfo.moves)
| WonByCheckmate (displayInfo, player) -> 
    match player with
    | Black -> sprintf "%s# 1-0" (movesToPGN displayInfo.moves)
    | White -> sprintf "%s# 0-1" (movesToPGN displayInfo.moves)
| GameStarted _ ->
    ""
| PlayerMoved (displayInfo, _) ->
    movesToPGN displayInfo.moves
| DrawOffer (displayInfo, _, _) ->
    sprintf "%s {Draw offered}" (movesToPGN displayInfo.moves)

let boardToFEN (b: Square[,]) =
    let rowFolder (list, empties) square =
        match square with
        | EmptySquare _ -> (list, empties + 1)
        | PieceSquare (piece, _) ->
            match empties with
            | 0 -> (piece.toFEN :: list, 0)
            | _ -> (piece.toFEN :: empties.ToString() :: list, 0)

    let rowResolve (list, empties) =
        match empties with
        | 0 -> list
        | _ -> empties.ToString() :: list

    let getRowSquares row = [|0..7|] |> Seq.map (fun i -> b.[row, i])

    let rowToFEN row =
        row
        |> getRowSquares
        |> Seq.fold rowFolder ([], 0)
        |> rowResolve
        |> Seq.rev
        |> String.concat ""

    [|0..7|]
    |> Seq.map rowToFEN
    |> String.concat "/"

type GameState with
    member this.toFEN =
        [
            boardToFEN (getDisplayInfo this).board
            this.statusToFEN
        ]
        |> String.concat " "

    member this.statusToFEN =
        let colorToFEN = function
        | White -> "w"
        | Black -> "b"

        let castleStatusToFEN (white:CastleStatus) (black:CastleStatus) =
            (if white.canCastleKingside then "K" else "") +
            (if white.canCastleQueenside then "Q" else "") +
            (if black.canCastleKingside then "k" else "") +
            (if black.canCastleKingside then "q" else "")

        let enPassantTarget = function
        | Some pos -> positionToAlgebraic pos
        | None -> "-"

        [
            colorToFEN this.turn
            castleStatusToFEN this.whitePlayerCastleState this.blackPlayerCastleState
            enPassantTarget this.pawnCapturableEnPassant
            this.pliesWithoutPawnOrCapture.ToString()
            this.numberOfMoves.ToString()
        ]
        |> String.concat " "

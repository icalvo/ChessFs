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
    $"%s{f.toAlgebraic}%s{r.toString}"

let squareToString = function
    | EmptySquare _ -> "  "
    | PieceSquare (Piece (color, shape), _) -> $"%s{color.toString}%s{shape.toString}"

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

let plyToAlgebraic plyOutput =
    let suffix = plyOutputSuffix plyOutput
    let ply = plyOutput.ply
    let sourcePos, targetPos = Ply.positions ply
    let targetPosString = positionToAlgebraic targetPos
    let sourceFile = (fst sourcePos)
    match ply with
    | Move (Piece (_, Pawn), _, _) ->
        $"%s{targetPosString}%s{suffix}"
    | Move (Piece (_, shape), _, _) ->
        $"%s{shape.toString}%s{targetPosString}%s{suffix}"
    | Capture (Piece (_, shape), _, _) ->
        match shape with
        | Pawn -> $"%s{sourceFile.toAlgebraic}x%s{targetPosString}%s{suffix}"
        | _ -> $"%s{shape.toString}x%s{targetPosString}"
    | CaptureEnPassant _ -> $"%s{sourceFile.toAlgebraic}x%s{targetPosString}%s{suffix}"
    | CastleKingSide _ -> $"O-O%s{suffix}"
    | CastleQueenSide _ -> $"O-O-O%s{suffix}"
    | Promote (_, _, _, targetShape) -> $"%s{targetPosString}=%s{targetShape.toString}%s{suffix}"
    | CaptureAndPromote (_, _, _, targetShape) -> $"%s{sourceFile.toAlgebraic}x%s{targetPosString}=%s{targetShape.toString}%s{suffix}"

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
        |[whitePly] -> $"%i{idx+1}. %s{plyToAlgebraic whitePly}"
        |[whitePly; blackPly] -> $"%i{idx+1}. %s{plyToAlgebraic whitePly} %s{plyToAlgebraic blackPly}"
        |_ -> "")
    >> String.concat " "

let outcomeToResult outcome =
    match outcome with
    | Draw (_, _, drawType) ->
        $"1/2-1/2 {{%A{drawType}}}"
    | DrawOffer _ ->
        "{Draw offered}"
    | DrawDeclinement _ ->
        "{Draw declined}"
    | LostByResignation (_, player)
    | WonByCheckmate (_, player) -> 
        match player with
        | Black -> "1-0"
        | White -> "0-1"
    | _ ->
        ""

let outcomeToSimpleResult outcome =
    match outcome with
    | Draw _ ->
        "1/2-1/2"
    | LostByResignation (_, player)
    | WonByCheckmate (_, player) -> 
        match player with
        | Black -> "1-0"
        | White -> "0-1"
    | _ ->
        ""

let replaceLastCheckByCheckmate (pgn: string) = $"%s{pgn.TrimEnd('+')}#"

let movesOutput (outcome: PlayerActionOutcome) =
    let pgn = movesToPGN outcome.displayInfo.moves
    match outcome with
    | WonByCheckmate _ -> replaceLastCheckByCheckmate pgn
    | _ -> pgn
    
let outcomeToPGN outcome =
    let outcomeRepr = outcomeToResult outcome
    let outcomeReprSeparator = if outcomeRepr = "" then "" else " "
    $"%s{movesOutput outcome}%s{outcomeReprSeparator}%s{outcomeRepr}"

let outcomeToSimplePGN outcome =
    $"%s{movesOutput outcome} %s{outcomeToSimpleResult outcome}"

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

        let castleStatusToFEN (white:CastlingRights) (black:CastlingRights) =
            (if white.canCastleKingSide then "K" else "") +
            (if white.canCastleQueenSide then "Q" else "") +
            (if black.canCastleKingSide then "k" else "") +
            (if black.canCastleKingSide then "q" else "")

        let enPassantTarget = function
        | Some pos -> positionToAlgebraic pos
        | None -> "-"

        [
            colorToFEN this.playerInTurn
            castleStatusToFEN this.whitePlayerCastlingRights this.blackPlayerCastlingRights
            enPassantTarget this.pawnCapturableEnPassant
            this.pliesWithoutPawnOrCapture.ToString()
            this.numberOfMoves.ToString()
        ]
        |> String.concat " "

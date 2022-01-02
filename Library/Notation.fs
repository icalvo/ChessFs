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

let fileToAlgebraic ((f, _):Coordinate) =
    $"%s{f.toAlgebraic}"

let rankToAlgebraic ((_, r):Coordinate) =
    $"%s{r.toString}"

let coordToAlgebraic ((f, r):Coordinate) =
    $"%s{f.toAlgebraic}%s{r.toString}"

let pieceToString = function
    | Piece (White, King) -> "♔"
    | Piece (White, Queen) -> "♕"
    | Piece (White, Rook) -> "♖"
    | Piece (White, Bishop) -> "♗"
    | Piece (White, Knight) -> "♘"
    | Piece (White, Pawn) -> "♙"
    | Piece (Black, King) -> "♚"
    | Piece (Black, Queen) -> "♛"
    | Piece (Black, Rook) -> "♜"
    | Piece (Black, Bishop) -> "♝"
    | Piece (Black, Knight) -> "♞"
    | Piece (Black, Pawn) -> "♟︎"

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
        |> Square.pieceCoord
        |> capabilityAt
        |> Option.map fst
        |> Option.map actionToString
        |> defaultArg <| " "

    (squareToString square) + squareActionToString

let generalSourceDiscriminator ambiguousPlies s =
    let otherAtSameRank = Seq.exists (fun x -> rank (Ply.source x) = rank s) ambiguousPlies
    let otherAtSameFile = Seq.exists (fun x -> file (Ply.source x) = file s) ambiguousPlies

    let rankDiscriminator =
        if Seq.isEmpty ambiguousPlies || (not otherAtSameRank && otherAtSameFile) then
            ""
        else
            fileToAlgebraic s
    let fileDiscriminator = if otherAtSameFile then rankToAlgebraic s else ""

    rankDiscriminator + fileDiscriminator

let moveSourceDiscriminator ambiguousPlies shape s =
    match shape with
    | Pawn
    | King -> ""
    | _ -> generalSourceDiscriminator ambiguousPlies s

let captureSourceDiscriminator ambiguousPlies shape s =
    match shape with
    | Pawn -> fileToAlgebraic s
    | King -> ""
    | _ -> generalSourceDiscriminator ambiguousPlies s

let shapeString shape =
    match shape with
    | Pawn -> ""
    | _ -> shape.toString

let plyToAlgebraic (plyOutput: PlyOutput) =
    let checkSuffix = if plyOutput.isCheck then "+" else ""
    let ply = plyOutput.ply
    let restOfPlies = plyOutput.restOfPlies
    let ambiguousPlies =
        restOfPlies
        |> Seq.where (fun p ->
            Ply.source  p <> Ply.source  ply &&
            Ply.plyType p =  Ply.plyType ply &&
            Ply.shape   p =  Ply.shape   ply &&
            Ply.target  p =  Ply.target  ply)

    let algebraicWithoutSuffix =
        match ply with
        | Move (Piece (_, shape), s, t) ->
            let sourceDiscriminator = moveSourceDiscriminator ambiguousPlies shape s
            let targetPosString = coordToAlgebraic t
            
            (shapeString shape) + sourceDiscriminator + targetPosString
        | Capture (Piece (_, shape), s, t) ->
            let sourceDiscriminator = captureSourceDiscriminator ambiguousPlies shape s
            let targetPosString = coordToAlgebraic t
            (shapeString shape) + sourceDiscriminator + "x" + targetPosString
        | CaptureEnPassant (_, s, t) ->
            let sourceDiscriminator = captureSourceDiscriminator ambiguousPlies Pawn s
            let targetPosString = coordToAlgebraic t
            (shapeString Pawn) + sourceDiscriminator + "x" + targetPosString
        | CastleKingSide _ -> "O-O"
        | CastleQueenSide _ -> "O-O-O"
        | Promote (_, _, t, targetShape) ->
            let targetPosString = coordToAlgebraic t
            $"%s{targetPosString}=%s{targetShape.toString}"
        | CaptureAndPromote (_, s, t, targetShape) ->
            let targetPosString = coordToAlgebraic t
            let rankDiscriminator = fileToAlgebraic s
            let sourceDiscriminator = rankDiscriminator
            $"%s{sourceDiscriminator}x%s{targetPosString}=%s{targetShape.toString}"

    algebraicWithoutSuffix + checkSuffix

let playerActionToAlgebraic = function
    | MovePiece (ply, restOfPlies, drawOffer) -> plyToAlgebraic { ply = ply; restOfPlies = restOfPlies; isCheck = false; drawOffer = false } + (if drawOffer then ":d" else "")
    | Resign -> ":r"
    | AcceptDraw -> ":a"

let printCoordinates = List.map coordToAlgebraic >> List.iter (printf "%A")

let executableActionToAlgebraic action = action |> ExecutableAction.action |> playerActionToAlgebraic

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
        |[whitePly] -> $"%i{idx+1}. %s{ plyToAlgebraic whitePly }"
        |[whitePly; blackPly] -> $"%i{idx+1}. %s{ plyToAlgebraic whitePly } %s{ plyToAlgebraic blackPly }"
        |_ -> "")
    >> String.concat " "

let outcomeToResultSeparator outcome =
    match outcome with
    | Draw _
    | PlayerMoved (_, _, true)
    | LostByResignation _ -> " "
    | _ -> ""

let outcomeToResult = function
    | Draw (_, _, drawType) ->
        $"1/2-1/2 {{%A{drawType}}}"
    | LostByResignation (_, player) ->
        match player with
        | Black -> "1-0"
        | White -> "0-1"
    | WonByCheckmate (_, player) ->
        match player with
        | Black -> "# 1-0"
        | White -> "# 0-1"
    | PlayerMoved (_, _, true) -> "{Draw offered}"
    | GameStarted _
    | PlayerMoved (_, _, false) -> ""

let outcomeToResultSuffix outcome =
    $"%s{outcomeToResultSeparator outcome}%s{outcomeToResult outcome}"

let outcomeToSimpleResultSuffix outcome =
    match outcome with
    | Draw _ ->
        " 1/2-1/2"
    | LostByResignation (_, player) ->
        match player with
        | Black -> " 1-0"
        | White -> " 0-1"
    | WonByCheckmate (_, player) ->
        match player with
        | Black -> "# 1-0"
        | White -> "# 0-1"
    | _ ->
        ""

let removeLastCheck (pgn: string) = pgn.TrimEnd('+')

let movesOutput (outcome: PlayerActionOutcome) =
    let pgn = movesToPGN (PlayerActionOutcome.representation outcome).moves
    match outcome with
    | WonByCheckmate _ -> removeLastCheck pgn
    | _ -> pgn

let outcomeToPGN outcome =
    match outcome with
    | GameStarted _ -> "{GameStarted}"
    | _ -> (movesOutput outcome) + (outcomeToResultSuffix outcome)

let outcomeToSimplePGN outcome = (movesOutput outcome) + (outcomeToSimpleResultSuffix outcome)

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


module ChessStateRepresentation =
    let statusToFEN (this: ChessStateRepresentation) =
        let colorToFEN = function
        | White -> "w"
        | Black -> "b"

        let castleStatusToFEN (white:CastlingRights) (black:CastlingRights) =
            (if white.canCastleKingSide then "K" else "") +
            (if white.canCastleQueenSide then "Q" else "") +
            (if black.canCastleKingSide then "k" else "") +
            (if black.canCastleKingSide then "q" else "")

        let enPassantTarget = function
        | Some coord -> coordToAlgebraic coord
        | None -> "-"

        [
            colorToFEN this.playerInTurn
            castleStatusToFEN this.whitePlayerCastlingRights this.blackPlayerCastlingRights
            enPassantTarget this.pawnCapturableEnPassant
            this.pliesWithoutPawnOrCapture.ToString()
            this.numberOfMoves.ToString()
        ]
        |> String.concat " "

    let toFEN (this: ChessStateRepresentation) =
        [
            boardToFEN this.board
            statusToFEN this
        ]
        |> String.concat " "

module ChessState =
    let internal toFEN = representation >> ChessStateRepresentation.toFEN
    let internal statusToFEN = representation >> ChessStateRepresentation.statusToFEN

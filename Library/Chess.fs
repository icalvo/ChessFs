namespace ChessFs.Chess

// CORE TYPES
type Shape =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King

type Rank =
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
    member x.toInt =
        match x with
        | R1 -> 7
        | R2 -> 6
        | R3 -> 5
        | R4 -> 4
        | R5 -> 3
        | R6 -> 2
        | R7 -> 1
        | R8 -> 0

type CreateRank = int -> Rank option

type File = A | B | C | D | E | F | G | H
with
    member x.toInt =
        match x with
        | A -> 0
        | B -> 1
        | C -> 2
        | D -> 3
        | E -> 4
        | F -> 5
        | G -> 6
        | H -> 7

type Coordinate = File * Rank

type Color = Black | White

type Player = Player of Color

type Colored<'T> = {
    white: 'T
    black: 'T
}
        
type Piece = Piece of Color * Shape

type BoardChange =
    | MovePiece of Coordinate * Coordinate
    | RemovePiece of Coordinate
    | AddPiece of Color * Shape * Coordinate

/// <summary>
/// A ply represents a piece move made by one of the players.
/// </summary>
type Ply =
    | Move of Piece * Coordinate * Coordinate
    | Capture of Piece * Coordinate * Coordinate
    | CaptureEnPassant of Color * Coordinate * Coordinate
    | CastleKingSide of Color
    | CastleQueenSide of Color
    | MoveAndPromote of Color * Coordinate * Coordinate * Shape
    | CaptureAndPromote of Color * Coordinate * Coordinate * Shape

type PieceReach = seq<Coordinate>

type PieceReaches = seq<Coordinate> list

type Square = 
    | PieceSquare of Piece * Coordinate
    | EmptySquare of Coordinate

type CastlingRights = {
    canCastleKingSide: bool
    canCastleQueenSide: bool
}

type RepetitionState = {
    turn: Player
    castlingRights: Colored<CastlingRights>
    pawnCapturableEnPassant: Coordinate option
    pieces: Map<File * Rank, Piece>
}

type CheckStatus =
    | IsCheck
    | NoCheck

type DrawOfferStatus =
    | IsDrawOffer
    | NoDrawOffer

type PlyOutput = {
    ply: Ply
    checkStatus: CheckStatus
    restOfPlies: Ply list
    drawOffer: DrawOfferStatus
}

type RawChessState = {
    playerInTurn: Player
    castlingRights: Colored<CastlingRights>
    pawnCapturableEnPassant: Coordinate option
    pieces: Map<File * Rank, Piece>
    pliesWithoutPawnOrCapture: int
    numberOfMoves: int
}

type internal ReachStep =
    | PossiblePly of Ply
    | PossibleFinalPly of Ply
    | Intermediate
    | Impossible

/// <summary>
/// Valid chess state
/// </summary>
/// <remarks>
/// In chess, the state of the game is defined by several properties in addition
/// to the pieces on the board. We need to know the castling rights of each player,
/// whether the last move allows en passant capture (and the coordinate of the pawn that
/// can be captured), the repeated coordinates and their number, and the number of
/// moves since a pawn moved or a capture happened.
///
/// This type represents a valid chess state, meaning that it will never hold state that has not been validated.
/// That's why this type is private; instead you will need to use constructors defined elsewhere.
/// </remarks>
[<CustomEquality; NoComparison>]
type ChessState = private {
    playerInTurn: Player
    castlingRights: Colored<CastlingRights>
    pawnCapturableEnPassant: Coordinate option
    pieces: Map<File * Rank, Piece>
    plies: PlyOutput list
    pliesWithoutPawnOrCapture: int
    repeatableStates: RepetitionState list
    numberOfMoves: int
    kings: Colored<Coordinate>
}
with
    member this.toRaw = {
        pieces = this.pieces
        playerInTurn = this.playerInTurn
        castlingRights = this.castlingRights
        pawnCapturableEnPassant = this.pawnCapturableEnPassant
        pliesWithoutPawnOrCapture = this.pliesWithoutPawnOrCapture
        numberOfMoves = this.numberOfMoves
    }
    override this.Equals other =
        match other with
        | :? ChessState as p -> p.toRaw.Equals this.toRaw
        | _ -> false
    override this.GetHashCode () = this.toRaw.GetHashCode()    

type DrawType =
    | Agreement
    | Stalemate
    | FiftyMovements
    | ThreefoldRepetition
    | FivefoldRepetition
    | SeventyFiveMovements
    | InsufficientMaterial

/// <summary>
/// Possible results of a player action result.
/// </summary>
type PlayerActionOutcome =
    | GameStarted of ChessState * ExecutableAction list
    | DrawOffered of ChessState * ExecutableAction list
    | PlayerMoved of ChessState * ExecutableAction list
    | WonByCheckmate of ChessState * Player
    | LostByResignation of ChessState * Player
    | Draw of ChessState * DrawType

and ExecutableAction = PlayerAction * (unit -> PlayerActionOutcome)

and PlayerAction =
    | MovePiece of Ply * Ply list * DrawOfferStatus
    | Resign
    | AcceptDraw

type ValidationError =
| Parsing of string
| MoreThanOneKing of Color
| MoreThanEightPawns of Color
| MoreExcessPiecesThanAbsentPawns of Color


module Coordinate =
    let file ((f, _):Coordinate) = f

    let rank ((_, r):Coordinate) = r

    let A1 = (A, R1)
    let A2 = (A, R2)
    let A3 = (A, R3)
    let A4 = (A, R4)
    let A5 = (A, R5)
    let A6 = (A, R6)
    let A7 = (A, R7)
    let A8 = (A, R8)
    let B1 = (B, R1)
    let B2 = (B, R2)
    let B3 = (B, R3)
    let B4 = (B, R4)
    let B5 = (B, R5)
    let B6 = (B, R6)
    let B7 = (B, R7)
    let B8 = (B, R8)
    let C1 = (C, R1)
    let C2 = (C, R2)
    let C3 = (C, R3)
    let C4 = (C, R4)
    let C5 = (C, R5)
    let C6 = (C, R6)
    let C7 = (C, R7)
    let C8 = (C, R8)
    let D1 = (D, R1)
    let D2 = (D, R2)
    let D3 = (D, R3)
    let D4 = (D, R4)
    let D5 = (D, R5)
    let D6 = (D, R6)
    let D7 = (D, R7)
    let D8 = (D, R8)
    let E1 = (E, R1)
    let E2 = (E, R2)
    let E3 = (E, R3)
    let E4 = (E, R4)
    let E5 = (E, R5)
    let E6 = (E, R6)
    let E7 = (E, R7)
    let E8 = (E, R8)
    let F1 = (F, R1)
    let F2 = (F, R2)
    let F3 = (F, R3)
    let F4 = (F, R4)
    let F5 = (F, R5)
    let F6 = (F, R6)
    let F7 = (F, R7)
    let F8 = (F, R8)
    let G1 = (G, R1)
    let G2 = (G, R2)
    let G3 = (G, R3)
    let G4 = (G, R4)
    let G5 = (G, R5)
    let G6 = (G, R6)
    let G7 = (G, R7)
    let G8 = (G, R8)
    let H1 = (H, R1)
    let H2 = (H, R2)
    let H3 = (H, R3)
    let H4 = (H, R4)
    let H5 = (H, R5)
    let H6 = (H, R6)
    let H7 = (H, R7)
    let H8 = (H, R8)

    let nextRank: Coordinate -> Coordinate option = function
        | f, R1 -> Some (f, R2)
        | f, R2 -> Some (f, R3)
        | f, R3 -> Some (f, R4)
        | f, R4 -> Some (f, R5)
        | f, R5 -> Some (f, R6)
        | f, R6 -> Some (f, R7)
        | f, R7 -> Some (f, R8)
        | _, R8 -> None

    let prevRank: Coordinate -> Coordinate option = function
        | _, R1 -> None
        | f, R2 -> Some (f, R1)
        | f, R3 -> Some (f, R2)
        | f, R4 -> Some (f, R3)
        | f, R5 -> Some (f, R4)
        | f, R6 -> Some (f, R5)
        | f, R7 -> Some (f, R6)
        | f, R8 -> Some (f, R7)

    let nextFile: Coordinate -> Coordinate option = function
        | A, r -> Some (B, r)
        | B, r -> Some (C, r)
        | C, r -> Some (D, r)
        | D, r -> Some (E, r)
        | E, r -> Some (F, r)
        | F, r -> Some (G, r)
        | G, r -> Some (H, r)
        | H, _ -> None

    let prevFile: Coordinate -> Coordinate option = function
        | A, _ -> None
        | B, r -> Some (A, r)
        | C, r -> Some (B, r)
        | D, r -> Some (C, r)
        | E, r -> Some (D, r)
        | F, r -> Some (E, r)
        | G, r -> Some (F, r)
        | H, r -> Some (G, r)

module Color =
    let opposite = function
        | White -> Black
        | Black -> White

module Colored =
    let get (c: Colored<'T>) color =
        match color with
        | White ->  c.white
        | Black ->  c.black
    let update color item (c: Colored<'T>) =
        match color with
        | White ->  {c with white = item }
        | Black ->  {c with black = item }

module Player =
    open Color

    let color = function | Player color -> color
    let opponent (Player playerColor) = Player (opposite playerColor)

module Piece =
    let WhitePawn = Piece (White, Pawn)
    let WhiteKnight = Piece (White, Knight)
    let WhiteBishop = Piece (White, Bishop)
    let WhiteRook = Piece (White, Rook)
    let WhiteQueen = Piece (White, Queen)
    let WhiteKing = Piece (White, King)
    let BlackPawn = Piece (Black, Pawn)
    let BlackKnight = Piece (Black, Knight)
    let BlackBishop = Piece (Black, Bishop)
    let BlackRook = Piece (Black, Rook)
    let BlackQueen = Piece (Black, Queen)
    let BlackKing = Piece (Black, King)

    let color (Piece (color, _)) = color
    let shape (Piece (_, shape)) = shape

module Ply =
    open Coordinate

    type PlyConstructor = Coordinate -> Piece -> Coordinate -> Ply

    let move src (Piece (color, sh)) tgt =
        let p = Piece (color, sh)
        Move (p, src, tgt)

    let capture src (Piece (color, sh)) tgt =
        let p = Piece (color, sh)
        Capture (p, src, tgt)

    let captureEnPassant src (Piece (color, _)) tgt =
        CaptureEnPassant (color, src, tgt)

    let castleKingSide _ (Piece (color, _)) _ =
        CastleKingSide color

    let castleQueenSide _ (Piece (color, _)) _ =
        CastleQueenSide color

    let moveAndPromote shape src (Piece (color, _)) tgt =
        MoveAndPromote (color, src, tgt, shape)
            
    let captureAndPromote shape src (Piece (color, _)) tgt =
        CaptureAndPromote (color, src, tgt, shape)

    let sameType one other =
        match one, other with
        | Move _, Move _
        | Capture _, Capture _
        | CaptureEnPassant _, CaptureEnPassant _
        | CastleKingSide _, CastleKingSide _
        | CastleQueenSide _, CastleQueenSide _
        | MoveAndPromote _, MoveAndPromote _
        | CaptureAndPromote _, CaptureAndPromote _ -> true
        | _ -> false

    let isCapture = function
        | Capture _
        | CaptureAndPromote _
        | CaptureEnPassant _ -> true
        | _ -> false
    let source = function
        | Move (_, source, _) -> source
        | Capture (_, source, _) -> source
        | CaptureEnPassant (_, source, _) -> source
        | CastleKingSide White
        | CastleQueenSide White -> E1
        | CastleKingSide Black
        | CastleQueenSide Black -> E8
        | MoveAndPromote (_, source, _, _) -> source
        | CaptureAndPromote (_, source, _, _) -> source
    let target = function
        | Move (_, _, target) -> target
        | Capture (_, _, target) -> target
        | CaptureEnPassant (_, _, target) -> target
        | CastleKingSide White -> G1
        | CastleQueenSide White -> C1
        | CastleKingSide Black -> G8
        | CastleQueenSide Black -> C8
        | MoveAndPromote (_, _, target, _) -> target
        | CaptureAndPromote (_, _, target, _) -> target
    let color = function
        | Move (Piece (color, _), _, _) -> color
        | Capture (Piece(color, _), _, _) -> color
        | CaptureEnPassant (color, _, _) -> color
        | CastleKingSide color -> color
        | CastleQueenSide color -> color
        | MoveAndPromote (color, _, _, _) -> color
        | CaptureAndPromote (color, _, _, _) -> color
    let equivalentMove = function
        | Move (Piece (color, shape), s, t) -> Move (Piece (color, shape), s, t)
        | Capture (Piece(color, shape), s, t) -> Move (Piece (color, shape), s, t)
        | CaptureEnPassant (color, s, t) -> Move (Piece (color, Pawn), s, t)
        | CastleKingSide color -> Move (Piece (color, King), (match color with | White -> E1 | Black -> E8), (match color with | White -> G1 | Black -> G8))
        | CastleQueenSide color -> Move (Piece (color, King), (match color with | White -> E1 | Black -> E8), (match color with | White -> C1 | Black -> C8))
        | MoveAndPromote (color, s, t, _) -> Move (Piece (color, Pawn), s, t)
        | CaptureAndPromote (color, s, t, _) -> Move (Piece (color, Pawn), s, t)
    let shape = function
        | Move (Piece (_, shape), _, _) -> shape
        | Capture (Piece(_, shape), _, _) -> shape
        | CaptureEnPassant _ -> Pawn
        | CastleKingSide _ -> King
        | CastleQueenSide _ -> King
        | MoveAndPromote _ -> Pawn
        | CaptureAndPromote _ -> Pawn
    let boardChanges = function
        | Move (_, source, target) -> [
                BoardChange.MovePiece (source, target)
            ]
        | Capture (_, source, target) -> [
            RemovePiece target;
            BoardChange.MovePiece (source, target)]
        | CaptureEnPassant (_, source, target) -> [
            let coordinateOfCapturedPawn = (file target, rank source)
            RemovePiece coordinateOfCapturedPawn
            BoardChange.MovePiece (source, target)]
        | CastleKingSide White -> [
                BoardChange.MovePiece (E1, G1);
                BoardChange.MovePiece (H1, F1)
            ]
        | CastleQueenSide White -> [
                BoardChange.MovePiece (E1, C1);
                BoardChange.MovePiece (A1, D1)
            ]
        | CastleKingSide Black -> [
                BoardChange.MovePiece (E8, G8);
                BoardChange.MovePiece (H8, F8)
            ]
        | CastleQueenSide Black -> [
                BoardChange.MovePiece (E8, C8);
                BoardChange.MovePiece (A8, D8)
            ]
        | MoveAndPromote (color, source, target, shape) -> [
                RemovePiece source;
                AddPiece (color, shape, target)
            ]
        | CaptureAndPromote (color, source, target, shape) -> [
                RemovePiece source;
                AddPiece (color, shape, target)
            ]

module Square =
    let isEmpty = function
        | EmptySquare _ -> true
        | _ -> false

    let hasPiece = function
        | PieceSquare _ -> true
        | _ -> false

    let pieceColor = function
        | PieceSquare (Piece (col, _), _) -> Some col
        | _ -> None

    let piece = function
        | PieceSquare (piece, _) -> Some piece
        | _ -> None

    let coordinate = function
        | PieceSquare (_, c) -> c
        | EmptySquare c -> c

    let pieceAndCoordinate: Square -> (Coordinate * Piece) option = function
        | PieceSquare (piece, coordinate) -> Some (coordinate, piece)
        | EmptySquare _ -> None

    let squareAt coordinate placedPieces =
        let found = placedPieces |> Map.tryFind coordinate

        match found with
        | Some x -> PieceSquare (x, coordinate)
        | None -> EmptySquare coordinate

    let (@@) placedPieces coordinate = squareAt coordinate placedPieces

    let sameColor (Piece (sourceColor, _)) square =
        match pieceColor square with
        | Some col -> col = sourceColor
        | None -> false
        
    let differentColor p s = not (sameColor p s)

module PlyOutput =
    let CheckPly (ply, restOfPlies, drawOffer) = { ply = ply; restOfPlies = restOfPlies; checkStatus = IsCheck; drawOffer = drawOffer }
    let RegularPly (ply, restOfPlies, drawOffer) = { ply = ply; restOfPlies = restOfPlies; checkStatus = NoCheck; drawOffer = drawOffer }

module CastlingRights =
    let canCastleKingSide (cr: CastlingRights) = cr.canCastleKingSide
    let canCastleQueenSide (cr: CastlingRights) = cr.canCastleQueenSide
    let justKingside = { canCastleKingSide = true; canCastleQueenSide = false }
    let justQueenside = { canCastleKingSide = false; canCastleQueenSide = true }
    let bothWays = { canCastleKingSide = true; canCastleQueenSide = true }
    let none = { canCastleKingSide = false; canCastleQueenSide = false }

module ChessState =
    open Square

    type ChessState with
        member this.king player =
            player |> Player.color |> Colored.get this.kings
        member this.currentPlayerCastlingRights =
            this.playerInTurn |> Player.color |> Colored.get this.castlingRights
        member this.repeatableState: RepetitionState = {
            turn = this.playerInTurn
            castlingRights = this.castlingRights
            pawnCapturableEnPassant = this.pawnCapturableEnPassant
            pieces = this.pieces
        }
        member this.repetitionCount =
            this.repeatableStates
            |> List.where (fun r -> this.repeatableState = r)
            |> List.length
        member this.board =
            let files = [A; B; C; D; E; F; G; H]
            let ranks = [
                R8
                R7
                R6
                R5
                R4
                R3
                R2
                R1
            ]        
            Array2D.init 8 8 (fun r f -> squareAt (files.[f], ranks.[r]) this.pieces)
        member this.moves = List.rev this.plies

    let board (rep: ChessState) = rep.board
    let playerInTurn (rep: ChessState) = rep.playerInTurn
    let whitePlayerCastlingRights (rep: ChessState) = rep.castlingRights.white
    let canWhiteCastleKingSide = whitePlayerCastlingRights >> CastlingRights.canCastleKingSide
    let canWhiteCastleQueenSide = whitePlayerCastlingRights >> CastlingRights.canCastleQueenSide
    let blackPlayerCastlingRights (rep: ChessState) = rep.castlingRights.black
    let canBlackCastleKingSide = blackPlayerCastlingRights >> CastlingRights.canCastleKingSide
    let canBlackCastleQueenSide = blackPlayerCastlingRights >> CastlingRights.canCastleQueenSide
    let pawnCapturableEnPassant (rep: ChessState) = rep.pawnCapturableEnPassant
    let pliesWithoutPawnOrCapture (rep: ChessState) = rep.pliesWithoutPawnOrCapture
    let moves (rep: ChessState) = rep.moves
    let numberOfMoves (rep: ChessState) = rep.numberOfMoves

    let internal (@@@) game coordinate = squareAt coordinate game.pieces

module internal ReachStep =
    let ply = function | PossiblePly p | PossibleFinalPly p -> Some p | _ -> None
    let isFinal = function | Impossible | PossibleFinalPly _ -> true | _ -> false

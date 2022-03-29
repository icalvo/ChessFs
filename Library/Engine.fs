module Engine

open System.Diagnostics
open Utils
open CoreTypes

type Colored<'T> = {
    white: 'T
    black: 'T
}

module Colored =
    let get color (c: Colored<'T>) =
        match color with
        | White ->  c.white
        | Black ->  c.black
    let update color item (c: Colored<'T>) =
        match color with
        | White ->  {c with white = item }
        | Black ->  {c with black = item }
        
type Piece = Piece of Color * Shape

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

module Piece =
    let color (Piece (color, _)) = color
    let shape (Piece (_, shape)) = shape

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

module Ply =
    
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
                MovePiece (source, target)
            ]
        | Capture (_, source, target) -> [
            RemovePiece target;
            MovePiece (source, target)]
        | CaptureEnPassant (_, source, target) -> [
            let coordinateOfCapturedPawn = (file target, rank source)
            RemovePiece coordinateOfCapturedPawn
            MovePiece (source, target)]
        | CastleKingSide White -> [
                MovePiece (E1, G1);
                MovePiece (H1, F1)
            ]
        | CastleQueenSide White -> [
                MovePiece (E1, C1);
                MovePiece (A1, D1)
            ]
        | CastleKingSide Black -> [
                MovePiece (E8, G8);
                MovePiece (H8, F8)
            ]
        | CastleQueenSide Black -> [
                MovePiece (E8, C8);
                MovePiece (A8, D8)
            ]
        | MoveAndPromote (color, source, target, shape) -> [
                RemovePiece source;
                AddPiece (color, shape, target)
            ]
        | CaptureAndPromote (color, source, target, shape) -> [
                RemovePiece source;
                AddPiece (color, shape, target)
            ]

type PieceReach = seq<Coordinate>

type PieceReaches = seq<Coordinate> list

module PieceReaches =
    open Ply

    // Directions
    let Up        = Rank.next
    let UpRight   = File.next >?> Rank.next
    let Right     = File.next
    let DownRight = File.next >?> Rank.prev
    let Down      = Rank.prev
    let DownLeft  = File.prev >?> Rank.prev
    let Left      = File.prev
    let UpLeft    = File.prev >?> Rank.next

    let bishopReaches coordinate: PieceReaches =
        [ UpRight; DownRight; DownLeft; UpLeft ]
        |> List.map Seq.unfoldSimple
        |> List.apply coordinate

    let rookReaches coordinate: PieceReaches =
        [ Up; Right; Down; Left ]
        |> List.map Seq.unfoldSimple
        |> List.apply coordinate

    let queenReaches coordinate: PieceReaches =
        [ bishopReaches; rookReaches ]
        |> List.apply coordinate
        |> List.concat

    let kingReaches coordinate: PieceReaches =
        [ UpRight; DownRight; DownLeft; UpLeft; Up; Right; Down; Left ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let knightReaches coordinate: PieceReaches =
        [
            Up >?> Up >?> Left;
            Up >?> Up >?> Right;
            Down >?> Down >?> Left;
            Down >?> Down >?> Right;
            Right >?> Right >?> Up;
            Right >?> Right >?> Down;
            Left >?> Left >?> Up;
            Left >?> Left >?> Down;
        ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let pawnSingleMoveReaches pawnDirection coordinate: PieceReaches =
        match pawnDirection coordinate with
        | Some newCoord -> newCoord |> Seq.singleton |> List.singleton
        | None -> []

    let pawnDoubleMoveReaches pawnDirection coordinate: PieceReaches =
        Seq.unfoldSimple pawnDirection coordinate
        |> Seq.take 2
        |> List.singleton

    let pawnCaptureReaches pawnDirection coordinate: PieceReaches =
        [
            pawnDirection >?> Left;
            pawnDirection >?> Right;
        ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToKingReach castlingDirection coordinate: PieceReaches =
        [ castlingDirection * 2; ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToQueenReach castlingDirection coordinate: PieceReaches =
        [ castlingDirection * 3; ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    type PawnRankType =
        | PromotionRank
        | EnPassantRank
        | DoubleMoveRank
        | RegularRank

    let pieceReaches piece coordinate =
        let applyReaches (reachesFunc: Coordinate -> PieceReaches, act: PlyConstructor) =
            coordinate
            |> reachesFunc
            |> List.where (not << Seq.isEmpty)
            |> List.map (Seq.map (act coordinate piece))

        let collectReaches = List.collect applyReaches

        match piece with
        | Piece (color, Pawn) ->
            let _, rank = coordinate
            let direction = match color with | White -> Up | Black -> Down

            let pawnRankType =
                match (color, rank) with
                | White, R7 | Black, R2 -> PromotionRank
                | White, R5 | Black, R4 -> EnPassantRank
                | White, R2 | Black, R7 -> DoubleMoveRank
                | _ -> RegularRank

            match pawnRankType with
            | PromotionRank ->
                let plyAndReachesFuncs = [
                    moveAndPromote, pawnSingleMoveReaches
                    captureAndPromote, pawnCaptureReaches 
                ]
                let promotionShapes = [ Queen; Rook; Knight; Bishop ]

                promotionShapes
                |> List.allPairs plyAndReachesFuncs
                |> List.map (fun ((plyFunc, reachesFunc), promotionShape) -> reachesFunc direction, plyFunc promotionShape)
                |> List.collect applyReaches
            | EnPassantRank ->
                collectReaches [
                    pawnSingleMoveReaches direction, move
                    pawnCaptureReaches    direction, capture
                    pawnCaptureReaches    direction, captureEnPassant]
            | DoubleMoveRank ->
                collectReaches [
                    pawnDoubleMoveReaches direction, move
                    pawnCaptureReaches    direction, capture
                ]
            | RegularRank ->
                collectReaches [
                    pawnSingleMoveReaches direction, move
                    pawnCaptureReaches    direction, capture
                ]
        | Piece (_, Knight)
        | Piece (_, Bishop)
        | Piece (_, Rook  )
        | Piece (_, Queen ) ->
                let reachesFunc =
                    match Piece.shape piece with
                    | Knight -> knightReaches
                    | Bishop -> bishopReaches
                    | Rook -> rookReaches
                    | Queen -> queenReaches
                    | _ -> fun _ -> []

                [ move; capture ]
                |> List.map (fun plyFunc -> reachesFunc, plyFunc)
                |> List.collect applyReaches            
        | Piece (color, King  ) ->
            let kingSideDirection = match color with | White -> Right | Black -> Left;
            let queenSideDirection = match color with | White -> Left | Black -> Right;
            collectReaches [
                (kingReaches, move)
                (kingReaches, capture)
                (kingCastleToKingReach  kingSideDirection , castleKingSide)
                (kingCastleToQueenReach queenSideDirection, castleQueenSide)
            ]

let opposite = function
    | White -> Black
    | Black -> White

type Square = 
    | PieceSquare of Piece * Coordinate
    | EmptySquare of Coordinate

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
    match Square.pieceColor square with
    | Some col -> col = sourceColor
    | None -> false
    
let differentColor p s = not (sameColor p s)

type CastlingRights = {
    canCastleKingSide: bool
    canCastleQueenSide: bool
}

module CastlingRights =
    let canCastleKingSide (cr: CastlingRights) = cr.canCastleKingSide
    let canCastleQueenSide (cr: CastlingRights) = cr.canCastleQueenSide
    let justKingside = { canCastleKingSide = true; canCastleQueenSide = false }
    let justQueenside = { canCastleKingSide = false; canCastleQueenSide = true }
    let bothWays = { canCastleKingSide = true; canCastleQueenSide = true }
    let none = { canCastleKingSide = false; canCastleQueenSide = false }

type RepetitionState = {
    turn: Player
    whitePlayerCastlingRights: CastlingRights
    blackPlayerCastlingRights: CastlingRights
    pawnCapturableEnPassant: Coordinate option
    pieces: Map<File * Rank, Piece>
}

/// <summary>
/// A ply produces an output that can be a check, a checkmate, or none (regular ply).
/// </summary>

type CheckStatus =
    | IsCheck
    | NoCheck

module CheckStatus =
    let create =
        function
        | true -> IsCheck
        | false -> NoCheck
    let toBool =
        function
        | IsCheck -> true
        | NoCheck -> false

type DrawOfferStatus =
    | IsDrawOffer
    | NoDrawOffer

type PlyOutput = {
    ply: Ply
    checkStatus: CheckStatus
    restOfPlies: Ply list
    drawOffer: DrawOfferStatus
}

let CheckPly (ply, restOfPlies, drawOffer) = { ply = ply; restOfPlies = restOfPlies; checkStatus = IsCheck; drawOffer = drawOffer }
let RegularPly (ply, restOfPlies, drawOffer) = { ply = ply; restOfPlies = restOfPlies; checkStatus = NoCheck; drawOffer = drawOffer }

type RawChessState = {
    playerInTurn: Player
    whitePlayerCastlingRights: CastlingRights
    blackPlayerCastlingRights: CastlingRights
    pawnCapturableEnPassant: Coordinate option
    pieces: Map<File * Rank, Piece>
    pliesWithoutPawnOrCapture: int
    numberOfMoves: int
}

/// <summary>
/// </summary>
/// <remarks>
/// In chess, the state of the game is defined by several properties in addition
/// to the pieces on the board. We need to know the castling rights of each player,
/// whether the last move allows en passant capture (and the coordinate of the pawn that
/// can be captured), the repeated coordinates and their number, and the number of
/// moves since a pawn moved or a capture happened.
/// </remarks>
[<CustomEquality; NoComparison>]
type ChessState = internal {
    playerInTurn: Player
    whitePlayerCastlingRights: CastlingRights
    blackPlayerCastlingRights: CastlingRights
    pawnCapturableEnPassant: Coordinate option
    pieces: Map<File * Rank, Piece>
    plies: PlyOutput list
    pliesWithoutPawnOrCapture: int
    repeatableStates: RepetitionState list
    numberOfMoves: int
    kings: Colored<Coordinate>
}

// custom check - compare against CustomerId only
with
    member this.currentPlayerCastlingRights = 
        match this.playerInTurn with
        | Player White -> this.whitePlayerCastlingRights
        | Player Black -> this.blackPlayerCastlingRights
    member this.repeatableState: RepetitionState = {
        turn = this.playerInTurn
        whitePlayerCastlingRights = this.whitePlayerCastlingRights
        blackPlayerCastlingRights = this.blackPlayerCastlingRights
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
    member this.toRaw = {
        pieces = this.pieces
        playerInTurn = this.playerInTurn
        whitePlayerCastlingRights = this.whitePlayerCastlingRights
        blackPlayerCastlingRights = this.blackPlayerCastlingRights
        pawnCapturableEnPassant = this.pawnCapturableEnPassant
        pliesWithoutPawnOrCapture = this.pliesWithoutPawnOrCapture
        numberOfMoves = this.numberOfMoves
    }

    override this.Equals other =
        match other with
        | :? ChessState as p -> p.toRaw.Equals this.toRaw
        | _ -> false
    override this.GetHashCode () = this.toRaw.GetHashCode()
    
let internal (@@@) game coordinate = squareAt coordinate game.pieces

type ChessState with
    member this.king player =
        match player with
        | Player color -> this.kings |> Colored.get color

/// <summary>
/// Can the ply type be legally executed for a piece with a given target coordinate?
/// </summary>
let internal executeReason game ply =
    seq {            
        let plyColor = Ply.color ply
        let targetCoordinate = Ply.target ply
        let targetSquare = game @@@ targetCoordinate
        let differentColor color square =
            match Square.pieceColor square with
            | Some White -> color = Black
            | Some Black -> color = White
            | None -> true

        match ply with
        | Move _ | MoveAndPromote _ ->
            if targetSquare |> (not << Square.isEmpty) then yield $"Cannot move to the occupied square %A{targetSquare}"
        | Capture _ | CaptureAndPromote _ ->
            if not (Square.hasPiece targetSquare) then yield "Cannot capture an empty square"
            if not (differentColor plyColor targetSquare) then yield "Cannot capture own pieces"
        | CaptureEnPassant _ ->
            match game.pawnCapturableEnPassant with
            | Some capturable -> if not (capturable = targetCoordinate) then yield "Cannot capture en passant if the target is not the enabled pawn" 
            | None -> yield "Cannot capture en passant if there is no enabled pawn"
        | CastleKingSide color ->
            match color with
            | White ->
                if not game.whitePlayerCastlingRights.canCastleKingSide then yield "Right to castle kingside is lost"
                if game @@@ F1 |> (not << Square.isEmpty) then yield "F1 is not empty" 
                if game @@@ G1 |> (not << Square.isEmpty) then yield "G1 is not empty"
            | Black ->
                if not game.whitePlayerCastlingRights.canCastleKingSide then yield "Right to castle kingside is lost"
                if game @@@ F8 |> (not << Square.isEmpty) then yield "F1 is not empty" 
                if game @@@ G1 |> (not << Square.isEmpty) then yield "G1 is not empty"
                if not game.blackPlayerCastlingRights.canCastleKingSide then yield "Right to castle kingside is lost"
                if game @@@ F8 |> (not << Square.isEmpty) then yield "F8 is not empty"
                if game @@@ G8 |> (not << Square.isEmpty) then yield "G8 is not empty" 
        | CastleQueenSide color ->
            match color with
            | White ->
                if not game.whitePlayerCastlingRights.canCastleQueenSide then yield "Right to castle queenside is lost"
                if game @@@ B1 |> (not << Square.isEmpty) then yield "B1 is not empty" 
                if game @@@ C1 |> (not << Square.isEmpty) then yield "C1 is not empty" 
                if game @@@ D1 |> (not << Square.isEmpty) then yield "D1 is not empty" 
            | Black ->
                if not game.blackPlayerCastlingRights.canCastleQueenSide then yield "Right to castle queenside is lost"
                if game @@@ B8 |> (not << Square.isEmpty) then yield "B8 is not empty" 
                if game @@@ C8 |> (not << Square.isEmpty) then yield "C8 is not empty" 
                if game @@@ D8 |> (not << Square.isEmpty) then yield "D8 is not empty" 
    }

/// <summary>
/// Can the ply type be legally executed for a piece with a given target coordinate?
/// </summary>
let internal canExecute game ply =
    let plyColor = Ply.color ply
    let targetCoordinate = Ply.target ply
    let targetSquare = game @@@ targetCoordinate
    let differentColor color square =
        match Square.pieceColor square with
        | Some White -> color = Black
        | Some Black -> color = White
        | None -> true

    let areAllTrueFor arg = Array.forall ((|>) arg)

    match ply with
    | Move _ | MoveAndPromote _ ->
        targetSquare |> Square.isEmpty
    | Capture _ | CaptureAndPromote _ ->
        [| Square.hasPiece; differentColor plyColor |]
        |> areAllTrueFor targetSquare
    | CaptureEnPassant _ ->
        match game.pawnCapturableEnPassant with
        | Some capturable -> capturable = targetCoordinate
        | None -> false
    | CastleKingSide color ->
        match color with
        | White ->
            game.whitePlayerCastlingRights.canCastleKingSide &&
            game @@@ F1 |> Square.isEmpty &&
            game @@@ G1 |> Square.isEmpty
        | Black ->
            game.blackPlayerCastlingRights.canCastleKingSide &&
            game @@@ F8 |> Square.isEmpty &&
            game @@@ G8 |> Square.isEmpty
    | CastleQueenSide color ->
        match color with
        | White ->
            game.whitePlayerCastlingRights.canCastleQueenSide &&
            game @@@ B1 |> Square.isEmpty &&
            game @@@ C1 |> Square.isEmpty &&
            game @@@ D1 |> Square.isEmpty
        | Black ->
            game.blackPlayerCastlingRights.canCastleQueenSide &&
            game @@@ B8 |> Square.isEmpty &&
            game @@@ C8 |> Square.isEmpty &&
            game @@@ D8 |> Square.isEmpty

type internal ReachStep =
    | PossiblePly of Ply
    | PossibleFinalPly of Ply
    | Intermediate
    | Impossible

module internal ReachStep =
    let ply = function | PossiblePly p | PossibleFinalPly p -> Some p | _ -> None
    let isFinal = function | Impossible | PossibleFinalPly _ -> true | _ -> false

let internal evaluateReachSquare game ply =
    let canDoPly = canExecute game ply
    let movePly = Ply.equivalentMove ply
    let canMove = canExecute game movePly
    if canDoPly then
        if Ply.isCapture ply then
            PossibleFinalPly ply
        else
            PossiblePly ply
    // Even if the ply cannot be executed in the current target coordinate, if you can move through it we will
    // consider it an intermediate step that can still let us get to a good target square for the ply.
    elif canMove then
        Intermediate
    else
        Impossible

let internal reachCapabilities game plies =
    let first = plies |> Seq.head
    
    let shape = first |> Ply.shape
    let color = first |> Ply.color
    let source = first |> Ply.source
    let isCapture = first |> Ply.isCapture
    if source = H5 && shape = Queen && color = White && isCapture then
        Debugger.Break()
    
    plies
    |> Seq.map (evaluateReachSquare game)
    |> Seq.takeWhileIncludingLast (not << ReachStep.isFinal)
    |> Seq.map ReachStep.ply
    |> Seq.filterNones

/// <summary>Legal plies for a piece (without taking into account the
/// out-of-check rule).</summary>
let internal pieceCapabilitiesWithoutCheckFilter game piece sourceCoordinate =
    let pieceReaches = PieceReaches.pieceReaches piece sourceCoordinate
    pieceReaches |> Seq.collect (reachCapabilities game)

/// <summary>Is the coordinate attacked by a player?</summary>
let internal isAttackedBy playerColor game targetCoordinate =
    let attacksBy (coordinate, piece) =
        let direction = match playerColor with | White -> PieceReaches.Up | Black -> PieceReaches.Down
        let reachesFunc =
            match Piece.shape piece with
            | Knight -> PieceReaches.knightReaches
            | Bishop -> PieceReaches.bishopReaches
            | Rook -> PieceReaches.rookReaches
            | Queen -> PieceReaches.queenReaches
            | Pawn -> PieceReaches.pawnCaptureReaches direction
            | _ -> fun _ -> []

        let reaches = reachesFunc coordinate
        let traverseReach = Seq.takeWhileIncludingLast ((@@@) game >> Square.isEmpty)
        reaches |> Seq.collect traverseReach

    let allReaches = [ PieceReaches.queenReaches; PieceReaches.knightReaches ]
    let potentialAttackSources =
        allReaches
        |> Seq.collect (fun x -> x targetCoordinate)
        |> Seq.collect id

    let hasPieceWithColor: Square -> bool = function
    | PieceSquare (Piece(color, _), _) -> color = playerColor
    | _ -> false

    let x: Square -> Coordinate seq =
        Square.pieceAndCoordinate
        >> (Option.map attacksBy)
        >> (Option.defaultValue Seq.empty)
    let attackedCoordinates =
        potentialAttackSources
        |> Seq.map ((@@@) game)
        |> Seq.where hasPieceWithColor
        |> Seq.collect x
        
    Seq.contains targetCoordinate attackedCoordinates

/// <summary>Executes a board change (unchecked).</summary>
let private rawBoardChange pieces boardChange =
    match boardChange with
    | MovePiece (sourceCoordinate, targetCoordinate) ->
        match pieces @@ sourceCoordinate with
        | EmptySquare _ ->
            failwith $"Trying to move a piece at %A{sourceCoordinate} but there is no piece there."
        | PieceSquare (piece, _) ->
            pieces
            |> Map.remove sourceCoordinate
            |> Map.remove targetCoordinate
            |> Map.add targetCoordinate piece
    | RemovePiece coordinate ->
        assert (pieces |> Map.containsKey coordinate)

        pieces
        |> Map.remove coordinate
    | AddPiece (col, shp, coordinate) ->
        pieces
        |> Map.remove coordinate
        |> Map.add coordinate (Piece (col, shp))

let opponent (Player playerColor) = Player (opposite playerColor)

/// <summary>Is the player in check?</summary>
let internal isCheck player (game: ChessState) =
    let (Player playerColor) = player
    let kingCoordinate = game.king player
    isAttackedBy (opposite playerColor) game kingCoordinate |> CheckStatus.create

type ChessState with
    member this.check = isCheck this.playerInTurn this

let private rawExecutePly ply pieces =
    ply
    |> Ply.boardChanges
    |> List.fold rawBoardChange pieces

let internal nextGameState ply restOfPlies drawOffer gameState =
    let plyPreventsWhiteCastleKingSide =
        match ply with
        | CastleKingSide White
        | CastleQueenSide White
        | Move    (Piece (White, King), _, _)
        | Move    (Piece (White, Rook), (H, R1), _)
        | Capture (Piece (White, Rook), _, _)
        | Capture (Piece (White, King), _, _)
        | Capture (_, _, (H, R1)) -> true
        | _ -> false
    let plyPreventsBlackCastleKingSide =
        match ply with
        | CastleKingSide Black
        | CastleQueenSide Black
        | Move    (Piece (Black, King), _, _)
        | Move    (Piece (Black, Rook), (H, R8), _)
        | Capture (Piece (Black, Rook), _, _)
        | Capture (Piece (Black, King), _, _)
        | Capture (_, _, (H, R8)) -> true
        | _ -> false
    let plyPreventsWhiteCastleQueenSide =
        match ply with
        | CastleKingSide White
        | CastleQueenSide White
        | Move    (Piece (White, King), _, _)
        | Move    (Piece (White, Rook), (A, R1), _)
        | Capture (Piece (White, Rook), _, _)
        | Capture (Piece (White, King), _, _)
        | Capture (_, _, (A, R1)) -> true
        | _ -> false
    let plyPreventsBlackCastleQueenSide =
        match ply with
        | CastleKingSide Black
        | CastleQueenSide Black
        | Move    (Piece (Black, King), _, _)
        | Move    (Piece (Black, Rook), (A, R8), _)
        | Capture (Piece (Black, Rook), _, _)
        | Capture (Piece (Black, King), _, _)
        | Capture (_, _, (A, R8)) -> true
        | _ -> false

    let hasStructureChanged = Ply.isCapture ply || Ply.shape ply = Pawn
    let nextKingPosition =
        match ply with
        | Move(Piece (color, King), _, target)
        | Capture(Piece (color, King), _, target) -> gameState.kings |> Colored.update color target
        | CastleKingSide White -> gameState.kings |> Colored.update White G1
        | CastleKingSide Black -> gameState.kings |> Colored.update Black G8
        | CastleQueenSide White -> gameState.kings |> Colored.update White C1
        | CastleQueenSide Black -> gameState.kings |> Colored.update Black C8
        | _ -> gameState.kings

    let nextGameStateTemp = {
        kings = nextKingPosition
        playerInTurn = opponent gameState.playerInTurn
        pieces = gameState.pieces |> rawExecutePly ply
        pawnCapturableEnPassant =
            match ply with
            | Move (Piece (White, Pawn), (_, R2), (f, R4)) -> Some (f, R3)
            | Move (Piece (Black, Pawn), (_, R7), (f, R5)) -> Some (f, R6)
            | _ -> None
        blackPlayerCastlingRights =
            {
                canCastleKingSide =
                    gameState.blackPlayerCastlingRights.canCastleKingSide &&
                    not plyPreventsBlackCastleKingSide
                canCastleQueenSide =
                    gameState.blackPlayerCastlingRights.canCastleQueenSide &&
                    not plyPreventsBlackCastleQueenSide
            }
        whitePlayerCastlingRights =
            {
                canCastleKingSide =
                    gameState.whitePlayerCastlingRights.canCastleKingSide &&
                    not plyPreventsWhiteCastleKingSide
                canCastleQueenSide =
                    gameState.whitePlayerCastlingRights.canCastleQueenSide &&
                    not plyPreventsWhiteCastleQueenSide
            }
        plies = gameState.plies
        pliesWithoutPawnOrCapture = if hasStructureChanged then 0 else gameState.pliesWithoutPawnOrCapture + 1
        repeatableStates = if hasStructureChanged then [] else gameState.repeatableState :: gameState.repeatableStates
        numberOfMoves =
            match gameState.playerInTurn with
            | Player White -> gameState.numberOfMoves
            | Player Black -> gameState.numberOfMoves + 1
    }

    let isInCheck = isCheck nextGameStateTemp.playerInTurn nextGameStateTemp

    let newPlyOutput = {
        ply = ply
        restOfPlies = restOfPlies
        checkStatus = isInCheck
        drawOffer = drawOffer
    }
    { nextGameStateTemp with
        plies = newPlyOutput :: nextGameStateTemp.plies
    }

let internal pieceCapabilities game (piece, sourceCoordinate) =
    let isCheck p g = CheckStatus.toBool <| isCheck p g

    let runsIntoCheck ply =
        nextGameState ply [] NoDrawOffer game
        |> isCheck game.playerInTurn

    let castlingPathAttacked =
        function
        | CastleKingSide White -> isAttackedBy Black game F1
        | CastleKingSide Black -> isAttackedBy White game F8
        | CastleQueenSide White -> isAttackedBy Black game D1
        | CastleQueenSide Black -> isAttackedBy White game D8
        | _ -> false

    pieceCapabilitiesWithoutCheckFilter game piece sourceCoordinate
    |> Seq.where (not << castlingPathAttacked)
    |> Seq.where (not << runsIntoCheck)

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

module PlayerActionOutcome =
    let state = function
        | Draw (state, _)
        | LostByResignation (state, _)
        | WonByCheckmate (state, _)
        | GameStarted (state, _)
        | PlayerMoved (state, _)
        | DrawOffered (state, _) -> state

    let actions = function
        | Draw _
        | LostByResignation _
        | WonByCheckmate _ -> 
            []
        | GameStarted (_, availableActions)
        | DrawOffered (_, availableActions)
        | PlayerMoved (_, availableActions) ->
            availableActions

module ExecutableAction =
    let action: ExecutableAction -> PlayerAction = fst
    let executeFn: ExecutableAction -> unit -> PlayerActionOutcome = snd

// Possible plies for the player in turn.
let internal playerPlies gameState =
    let getCapabilities (sourceCoordinate, sourcePiece) =
        pieceCapabilities gameState (sourcePiece, sourceCoordinate)
    let (Player playerColor) = gameState.playerInTurn
    let belongsToPlayer _ (Piece (pieceColor, _)) = pieceColor = playerColor
    
    gameState.pieces
    |> Map.filter belongsToPlayer
    |> Map.toSeq
    |> Seq.collect getCapabilities

// given a player & a gameState, it returns a move result for that player.
let rec private recStart = ()

// player makes a move
and internal executePlayerAction (gameState: ChessState) (playerAction: PlayerAction): PlayerActionOutcome =
    match playerAction with
    | MovePiece (ply, restOfPlies, drawOffer) ->
        let newGameState = nextGameState ply restOfPlies drawOffer gameState
        getOutcomeFromNewBoard newGameState
    | Resign ->
        LostByResignation (gameState, (opponent gameState.playerInTurn))
    | AcceptDraw ->
        Draw (gameState, Agreement)

and internal getOutcomeFromNewBoard (gameState: ChessState) =
    let drawOffer =
        gameState.plies
        |> List.tryHead
        |> Option.map (fun x -> x.drawOffer)
        |> Option.defaultValue NoDrawOffer

    let isDrawOffer =
        match drawOffer with
        | IsDrawOffer -> true
        | NoDrawOffer -> false
        
    if isDrawOffer && gameState.pliesWithoutPawnOrCapture >= 50 then
        Draw (gameState, FiftyMovements)
    elif isDrawOffer &&  gameState.repetitionCount >= 3 then
        Draw (gameState, ThreefoldRepetition)
    elif gameState.pliesWithoutPawnOrCapture > 75 then
        Draw (gameState, SeventyFiveMovements)
    elif gameState.repetitionCount >= 5 then
        Draw (gameState, FivefoldRepetition)
    else
        let playerPlies = playerPlies gameState |> Seq.toList
        let canMove = playerPlies |> (not << List.isEmpty)
        if canMove then
            let actions = getExecutableActions playerPlies gameState drawOffer
            match (gameState.plies |> List.length, drawOffer) with
            | 0, _ -> GameStarted (gameState, actions)
            | _, IsDrawOffer -> DrawOffered (gameState, actions)
            | _, NoDrawOffer -> PlayerMoved (gameState, actions)
        else
            match isCheck gameState.playerInTurn gameState with
            | IsCheck -> WonByCheckmate (gameState, gameState.playerInTurn)
            | NoCheck -> Draw (gameState, Stalemate)

// Convert possible plies to executable actions, adding Resign and OfferDraw
and internal getExecutableActions plies gameState drawOffer =
    let movePieces = plies |> Seq.collect (fun ply -> [
        MovePiece (ply, plies, NoDrawOffer)
        MovePiece (ply, plies, IsDrawOffer)
    ])

    movePieces
    |> Seq.append [Resign]
    |> Seq.append (match drawOffer with | IsDrawOffer -> [AcceptDraw] | _ -> [])
    |> Seq.map (makeNextExecutableAction gameState)
    |> Seq.toList

and internal makeNextExecutableAction gameState playerAction =
    let executeFn() = executePlayerAction gameState playerAction
    playerAction, executeFn

module ChessState =
    let initial =
        let initialPieces = Map.ofList [
            A1, WhiteRook
            B1, WhiteKnight
            C1, WhiteBishop
            D1, WhiteQueen
            E1, WhiteKing
            F1, WhiteBishop
            G1, WhiteKnight
            H1, WhiteRook
            A2, WhitePawn
            B2, WhitePawn
            C2, WhitePawn
            D2, WhitePawn
            E2, WhitePawn
            F2, WhitePawn
            G2, WhitePawn
            H2, WhitePawn
            A7, BlackPawn
            B7, BlackPawn
            C7, BlackPawn
            D7, BlackPawn
            E7, BlackPawn
            F7, BlackPawn
            G7, BlackPawn
            H7, BlackPawn
            A8, BlackRook
            B8, BlackKnight
            C8, BlackBishop
            D8, BlackQueen
            E8, BlackKing
            F8, BlackBishop
            G8, BlackKnight
            H8, BlackRook
        ]

        {
            playerInTurn = Player White
            pieces = initialPieces
            whitePlayerCastlingRights = CastlingRights.bothWays
            blackPlayerCastlingRights = CastlingRights.bothWays
            pawnCapturableEnPassant = None
            plies = []
            pliesWithoutPawnOrCapture = 0
            repeatableStates = []
            numberOfMoves = 1
            kings = { white = E1; black = E8 }
        }
        
    /// constructor
    let board (rep: ChessState) = rep.board
    let playerInTurn (rep: ChessState) = rep.playerInTurn
    let check (rep: ChessState) = rep.check
    let whitePlayerCastlingRights (rep: ChessState) = rep.whitePlayerCastlingRights
    let canWhiteCastleKingSide = whitePlayerCastlingRights >> CastlingRights.canCastleKingSide
    let canWhiteCastleQueenSide = whitePlayerCastlingRights >> CastlingRights.canCastleQueenSide
    let blackPlayerCastlingRights (rep: ChessState) = rep.blackPlayerCastlingRights
    let canBlackCastleKingSide = blackPlayerCastlingRights >> CastlingRights.canCastleKingSide
    let canBlackCastleQueenSide = blackPlayerCastlingRights >> CastlingRights.canCastleQueenSide
    let pawnCapturableEnPassant (rep: ChessState) = rep.pawnCapturableEnPassant
    let pliesWithoutPawnOrCapture (rep: ChessState) = rep.pliesWithoutPawnOrCapture
    let moves (rep: ChessState) = rep.moves
    let numberOfMoves (rep: ChessState) = rep.numberOfMoves

let nullValidate = Ok

module Result =
    let ofOption errorData = function
        | Some x -> Ok x
        | None -> Error errorData

    let ofBool errorData = function
    | true -> Ok ()
    | false -> Error errorData

let internal validateStandardChess (gameState: ChessState) =
    let pieceMap (gameState: ChessState) =
        gameState.pieces
        |> Map.toList
        |> List.groupBy snd
        |> List.map (fun (k, pieces) -> (k, List.map (fun (coordinate, _) -> coordinate) pieces))
        |> Map.ofList

    let pieceCoords p = Map.tryFind p >> Result.ofOption $"No piece %A{p}"

    let length = List.length >> Some
    let thereIsOnlyPiece = List.length >> (fun x -> x = 1) >> Result.ofBool "There are more than one"


    let (?>>) f1 f2 = f1 >> Option.map f2
    let query f2 = function
    | Some x -> f2 x
    | None -> false
    
    let onlyOnePiece p =
        gameState |> (pieceMap >> Map.tryFind p ?>> List.length >> query (fun x -> x = 1))

    let nPiecesOrLess p n =
        gameState |> (pieceMap >> Map.tryFind p ?>> List.length >> query (fun x -> x <= n))
    
    let pieceCoordinateIs piece coordinate =
        pieceMap >> Map.tryFind piece >> query (List.contains coordinate)

    let whiteCastlingKingside =
        (not gameState.whitePlayerCastlingRights.canCastleKingSide) ||
        (
        gameState |> (pieceCoordinateIs WhiteKing E1) &&
        gameState |> (pieceCoordinateIs WhiteRook H1))

    let blackCastlingKingside =
        (not gameState.blackPlayerCastlingRights.canCastleKingSide) ||
        (
        gameState |> (pieceCoordinateIs BlackKing E8) &&
        gameState |> (pieceCoordinateIs BlackRook H8))

    let numberOfWhitePawns = 8
    let numberOfWhiteRooks = 2
    let numberOfWhiteKnights = 2
    let numberOfWhiteBishops = 2
    let numberOfWhiteQueens = 1

    let eightWhitePawnsOrLess = numberOfWhitePawns <= 8

    let excessWhitePieces =
        min (numberOfWhiteRooks - 2) 0 +
        min (numberOfWhiteKnights - 2) 0 +
        min (numberOfWhiteBishops - 2) 0 +
        min (numberOfWhiteQueens - 1) 0
    let numberOfCapturedWhitePawns = 8 - numberOfWhitePawns
    
    let moreExcessPiecesThanAllowedByCapturedPawns = numberOfCapturedWhitePawns >= excessWhitePieces

    let noPawnsAtFirstRank = true
    let noPawnsAtLastRank = true

    if (not <| onlyOnePiece WhiteKing) then
        Error "Not 1 white king"
    elif (not <| onlyOnePiece BlackKing) then
        Error "Not 1 black king"
    elif (not whiteCastlingKingside) then
        Error "White can castle kingside but the king is not at E1 or a rook at H1"
    elif (not blackCastlingKingside) then
        Error "Black can castle kingside but the king is not at E8 or a rook at H8"
    else
        Ok gameState


let setupChessPosition gameState validate =
    gameState
    |> validate
    |> Result.map getOutcomeFromNewBoard

let setupStandardChessPosition gameState =
    setupChessPosition gameState validateStandardChess

let initialStandardChessPosition () =
    ChessState.initial |> getOutcomeFromNewBoard

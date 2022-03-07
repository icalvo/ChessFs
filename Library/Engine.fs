module Engine

open Utils
open CoreTypes

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

type PlyType =
    | MoveType
    | CaptureType
    | EnPassantType
    | CastleKingSideType
    | CastleQueenSideType
    | MoveAndPromoteType
    | CaptureAndPromoteType

type Reach = seq<Coordinate> list

module Reach =
    // Directions
    let Up        = Rank.next
    let UpRight   = File.next >?> Rank.next
    let Right     = File.next
    let DownRight = File.next >?> Rank.prev
    let Down      = Rank.prev
    let DownLeft  = File.prev >?> Rank.prev
    let Left      = File.prev
    let UpLeft    = File.prev >?> Rank.next

    let bishopReaches coordinate: Reach =
        [ UpRight; DownRight; DownLeft; UpLeft ]
        |> List.map Seq.unfoldSimple
        |> List.apply coordinate

    let rookReaches coordinate: Reach =
        [ Up; Right; Down; Left ]
        |> List.map Seq.unfoldSimple
        |> List.apply coordinate

    let queenReaches coordinate: Reach =
        [ bishopReaches; rookReaches ]
        |> List.apply coordinate
        |> List.concat

    let kingReaches coordinate: Reach =
        [ UpRight; DownRight; DownLeft; UpLeft; Up; Right; Down; Left ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let knightReaches coordinate: Reach =
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

    let pawnSingleMoveReaches pawnDirection coordinate: Reach =
        match pawnDirection coordinate with
        | Some newCoord -> newCoord |> Seq.singleton |> List.singleton
        | None -> []

    let pawnDoubleMoveReaches pawnDirection coordinate: Reach =
        Seq.unfoldSimple pawnDirection coordinate
        |> Seq.take 2
        |> List.singleton

    let pawnCaptureReaches pawnDirection coordinate: Reach =
        [
            pawnDirection >?> Left;
            pawnDirection >?> Right;
        ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToKingReach castlingDirection coordinate: Reach =
        [ castlingDirection * 2; ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToQueenReach castlingDirection coordinate: Reach =
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
        let applyReaches (reachesFunc, act: PlyType) =
            coordinate
            |> reachesFunc
            |> List.where (not << Seq.isEmpty)
            |> List.map (fun reach -> (reach, act))

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
                collectReaches [
                    (pawnSingleMoveReaches direction, MoveAndPromoteType);
                    (pawnCaptureReaches    direction, CaptureAndPromoteType)]
            | EnPassantRank ->
                collectReaches [
                    (pawnSingleMoveReaches direction, MoveType);
                    (pawnCaptureReaches    direction, CaptureType);
                    (pawnCaptureReaches    direction, EnPassantType)]
            | DoubleMoveRank ->
                collectReaches [
                    (pawnDoubleMoveReaches direction, MoveType   );
                    (pawnCaptureReaches    direction, CaptureType);
                ]
            | RegularRank ->
                collectReaches [
                    (pawnSingleMoveReaches direction, MoveType   );
                    (pawnCaptureReaches    direction, CaptureType);
                ]
        | Piece (_, Knight) ->
            collectReaches [
                (knightReaches, MoveType);
                (knightReaches, CaptureType)
            ]
        | Piece (_, Bishop) ->
            collectReaches [
                (bishopReaches, MoveType);
                (bishopReaches, CaptureType)
            ]
        | Piece (_, Rook  ) ->
            collectReaches [
                (rookReaches, MoveType);
                (rookReaches, CaptureType)
            ]
        | Piece (_, Queen ) ->
            collectReaches [
                (queenReaches, MoveType);
                (queenReaches, CaptureType)
            ]
        | Piece (White, King  ) ->
            collectReaches [
                (kingReaches, MoveType);
                (kingReaches, CaptureType);
                (kingCastleToKingReach  Right, CastleKingSideType);
                (kingCastleToQueenReach Left , CastleQueenSideType)
            ]
        | Piece (Black, King  ) ->
            collectReaches [
                (kingReaches, MoveType);
                (kingReaches, CaptureType);
                (kingCastleToKingReach  Left , CastleKingSideType);
                (kingCastleToQueenReach Right, CastleQueenSideType)
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

let squareAt coordinate placedPieces =
    let found = placedPieces |> Map.tryFind coordinate

    match found with
    | Some x -> PieceSquare (x, coordinate)
    | None -> EmptySquare coordinate

let (@@) placedPieces coordinate = squareAt coordinate placedPieces

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
    | Promote of Color * Coordinate * Coordinate * Shape
    | CaptureAndPromote of Color * Coordinate * Coordinate * Shape

module Ply =
    let plyType = function
        | Move _ -> MoveType
        | Capture _ -> CaptureType
        | CaptureEnPassant _ -> EnPassantType
        | CastleKingSide _ -> CastleKingSideType
        | CastleQueenSide _ -> CastleQueenSideType
        | Promote _ -> MoveAndPromoteType
        | CaptureAndPromote _ -> CaptureAndPromoteType
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
        | Promote (_, source, _, _) -> source
        | CaptureAndPromote (_, source, _, _) -> source
    let target = function
        | Move (_, _, target) -> target
        | Capture (_, _, target) -> target
        | CaptureEnPassant (_, _, target) -> target
        | CastleKingSide White -> G1
        | CastleQueenSide White -> C1
        | CastleKingSide Black -> G8
        | CastleQueenSide Black -> C8
        | Promote (_, _, target, _) -> target
        | CaptureAndPromote (_, _, target, _) -> target
    let shape = function
        | Move (Piece (_, shape), _, _) -> shape
        | Capture (Piece(_, shape), _, _) -> shape
        | CaptureEnPassant _ -> Pawn
        | CastleKingSide _ -> King
        | CastleQueenSide _ -> King
        | Promote _ -> Pawn
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
        | Promote (color, source, target, shape) -> [
                RemovePiece source;
                AddPiece (color, shape, target)
            ]
        | CaptureAndPromote (color, source, target, shape) -> [
                RemovePiece source;
                AddPiece (color, shape, target)
            ]

let coordinateOfCapturedPiece = function
    | Capture (_, _, target)
    | CaptureAndPromote (_, _, target, _) -> Some target
    | CaptureEnPassant (_, source, target) -> Some (file target, rank source)
    | _ -> None

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

let bothWaysCastlingRights = { canCastleKingSide = true; canCastleQueenSide = true }
let noCastlingRights = { canCastleKingSide = false; canCastleQueenSide = false }

type RepetitionState = {
    turn: Color
    whitePlayerCastlingRights: CastlingRights
    blackPlayerCastlingRights: CastlingRights
    pawnCapturableEnPassant: Coordinate option
    pieces: Map<File * Rank, Piece>
}

/// <summary>
/// A ply produces an output that can be a check, a checkmate, or none (regular ply).
/// </summary>
type PlyOutput = {
    ply: Ply
    isCheck: bool
    restOfPlies: Ply list
    drawOffer: bool
}

let CheckPly (ply, restOfPlies, drawOffer) = { ply = ply; restOfPlies = restOfPlies; isCheck = true; drawOffer = drawOffer }
let RegularPly (ply, restOfPlies, drawOffer) = { ply = ply; restOfPlies = restOfPlies; isCheck = false; drawOffer = drawOffer }

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
}

// custom check - compare against CustomerId only
with
    member this.currentPlayerCastlingRights = 
        match this.playerInTurn with
        | White -> this.whitePlayerCastlingRights
        | Black -> this.blackPlayerCastlingRights
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
    member this.board = Array2D.init 8 8 (fun r f -> squareAt (File.fromInt f, Rank.fromInt r) this.pieces)
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

/// <summary>
/// Generates plies from a ply type, a piece, and a source and target coordinates.
/// </summary>
/// <remarks>
/// In general, a single ply is generated. However, when there is a promotion, there
/// are 4 possible plies, one for each kind of promoted piece (=Q, =R, =B & =N).
/// </remarks>
let toPlies src (Piece (color, sh), plyType, tgt) =
    let p = Piece (color, sh)
    match plyType with
    | MoveType -> [ Move (p, src, tgt) ]
    | CaptureType -> [ Capture (p, src, tgt) ]
    | EnPassantType -> [ CaptureEnPassant (color, src, tgt) ]
    | CastleKingSideType -> [ CastleKingSide color ]
    | CastleQueenSideType -> [ CastleQueenSide color ]
    | MoveAndPromoteType -> [ Queen; Rook; Bishop; Knight ] |> List.map (fun shape -> Promote (color, src, tgt, shape))
    | CaptureAndPromoteType -> [ Queen; Rook; Bishop; Knight ] |> List.map (fun shape -> CaptureAndPromote (color, src, tgt, shape))

/// <summary>
/// Can the ply type be legally executed for a piece with a given target coordinate?
/// </summary>
let internal canExecute game sourcePiece targetCoordinate plyType =
    let targetSquare = game @@@ targetCoordinate

    match plyType with
    | MoveType | MoveAndPromoteType ->
        targetSquare |> Square.isEmpty
    | CaptureType | CaptureAndPromoteType ->
        targetSquare |> Square.hasPiece &&
        targetSquare |> (differentColor sourcePiece)
    | EnPassantType ->
        match game.pawnCapturableEnPassant with
        | Some capturable -> capturable = targetCoordinate
        | None -> false
    | CastleKingSideType ->
        match sourcePiece with
        | Piece (White, King) ->
            game.whitePlayerCastlingRights.canCastleKingSide &&
            game @@@ F1 |> Square.isEmpty &&
            game @@@ G1 |> Square.isEmpty
        | Piece (Black, King) ->
            game.blackPlayerCastlingRights.canCastleKingSide &&
            game @@@ F8 |> Square.isEmpty &&
            game @@@ G8 |> Square.isEmpty
        | _ -> false
    | CastleQueenSideType ->
        match sourcePiece with
        | Piece (White, King) ->
            game.whitePlayerCastlingRights.canCastleQueenSide &&
            game @@@ B1 |> Square.isEmpty &&
            game @@@ C1 |> Square.isEmpty &&
            game @@@ D1 |> Square.isEmpty
        | Piece (Black, King) ->
            game.blackPlayerCastlingRights.canCastleQueenSide &&
            game @@@ B8 |> Square.isEmpty &&
            game @@@ C8 |> Square.isEmpty &&
            game @@@ D8 |> Square.isEmpty
        | _ -> false

let internal evaluateReachSquare game plyTypeToTest piece targetCoordinate =
    let canDoPlyType = canExecute game piece targetCoordinate plyTypeToTest
    let canMove = canExecute game piece targetCoordinate MoveType
    if canDoPlyType then Some (piece, plyTypeToTest, targetCoordinate)
    // Even if the ply type cannot be executed, if you can move to the
    // target coordinate we still return something, so that the ply type
    // is evaluated further in the same reach.
    elif canMove then Some (piece, MoveType, targetCoordinate)
    else None

let internal reachCapabilities game piece (plyTypeToAnalyze: PlyType) reach =
    let actionIs plyTypeToCheck = function
        | Some (_, plyType, _) -> plyType = plyTypeToCheck
        | _ -> false

    reach
    |> Seq.map (evaluateReachSquare game plyTypeToAnalyze piece)
    |> Seq.takeWhileIncludingLast (actionIs MoveType)
    |> Seq.where (actionIs plyTypeToAnalyze)
    |> Seq.filterNones

/// <summary>Legal plies for a piece (without taking into account the
/// out-of-check rule).</summary>
let internal pieceCapabilitiesWithoutCheckFilter game piece sourceCoordinate =
    Reach.pieceReaches piece sourceCoordinate
    |> Seq.collect (fun (reach, plyType) -> reachCapabilities game piece plyType reach)
    |> Seq.collect (toPlies sourceCoordinate)

/// <summary>Coordinates attacked by a piece.</summary>
let internal attacksBy game (coordinate, piece) =
    pieceCapabilitiesWithoutCheckFilter game piece coordinate
    |> Seq.map coordinateOfCapturedPiece
    |> Seq.filterNones

/// <summary>Coordinates attacked by a player.</summary>
let internal attacks playerColor game =
    game.pieces
    |> Map.filter (fun _ (Piece (pieceColor, _)) -> pieceColor = playerColor)
    |> Map.toSeq
    |> Seq.collect (attacksBy game)

/// <summary>Is the coordinate attacked by a player?</summary>
let internal isAttackedBy playerColor game targetCoordinate =
    game
    |> attacks playerColor
    |> Seq.contains targetCoordinate

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

let opponent = opposite

/// <summary>Is the player in check?</summary>
let internal isCheck playerColor game =
    let possibleKingCoordinate = game.pieces |> Map.tryFindKey (fun _ piece -> piece = Piece (playerColor, King))
    match possibleKingCoordinate with
    | Some kingCoordinate -> isAttackedBy (opponent playerColor) game kingCoordinate
    | None -> false

type ChessState with
    member this.isCheck = isCheck this.playerInTurn this

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
    let nextGameStateTemp = {
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
            | White -> gameState.numberOfMoves
            | Black -> gameState.numberOfMoves + 1
    }

    let isInCheck = isCheck nextGameStateTemp.playerInTurn nextGameStateTemp

    let newPlyOutput = {
        ply = ply
        restOfPlies = restOfPlies
        isCheck = isInCheck
        drawOffer = drawOffer
    }
    { nextGameStateTemp with
        plies = newPlyOutput :: nextGameStateTemp.plies
    }

let internal pieceCapabilities game (piece, sourceCoordinate) =
    let checkFilter ply =
        nextGameState ply [] false game
        |> (not << isCheck game.playerInTurn)

    pieceCapabilitiesWithoutCheckFilter game piece sourceCoordinate
    |> Seq.where checkFilter

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
    | PlayerMoved of ChessState * ExecutableAction list * bool
    | WonByCheckmate of ChessState * Color
    | LostByResignation of ChessState * Color
    | Draw of ChessState * DrawType

and ExecutableAction = PlayerAction * (unit -> PlayerActionOutcome)

and PlayerAction =
    | MovePiece of Ply * Ply list * bool
    | Resign
    | AcceptDraw

module PlayerActionOutcome =
    let state = function
        | Draw (state, _) -> state
        | LostByResignation (state, _) -> state
        | WonByCheckmate (state, _) -> state
        | GameStarted (state, _) -> state
        | PlayerMoved (state, _, _) -> state

    let actions = function
        | Draw _
        | LostByResignation _
        | WonByCheckmate _ -> 
            []
        | GameStarted (_, availableActions)
        | PlayerMoved (_, availableActions, _) ->
            availableActions

module ExecutableAction =
    let action = fst
    let executefn = snd

// Possible plies for the player in turn.
let internal playerPlies gameState =
    let getCapabilities (sourceCoordinate, sourcePiece) =
        pieceCapabilities gameState (sourcePiece, sourceCoordinate)

    let belongsToPlayer _ (Piece (pieceColor, _)) = pieceColor = gameState.playerInTurn
    
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
    let drawOffer = gameState.plies |> List.tryHead |> Option.map (fun x -> x.drawOffer) |> Option.defaultValue false

    if drawOffer && gameState.pliesWithoutPawnOrCapture >= 50 then
        Draw (gameState, FiftyMovements)
    elif drawOffer &&  gameState.repetitionCount >= 3 then
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
            match gameState.plies |> List.length with
            | 0 -> GameStarted (gameState, actions)
            | _ -> PlayerMoved (gameState, actions, drawOffer)
        else if isCheck gameState.playerInTurn gameState then
            WonByCheckmate (gameState, gameState.playerInTurn)
        else
            Draw (gameState, Stalemate)

// Convert possible plies to executable actions, adding Resign and OfferDraw
and internal getExecutableActions plies gameState drawOffer =
    let movePieces = plies |> Seq.collect (fun ply -> [MovePiece (ply, plies, false); MovePiece (ply, plies, true)])

    movePieces
    |> Seq.append [Resign]
    |> Seq.append (if drawOffer then [AcceptDraw] else [])
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
            playerInTurn = White
            pieces = initialPieces
            whitePlayerCastlingRights = bothWaysCastlingRights
            blackPlayerCastlingRights = bothWaysCastlingRights
            pawnCapturableEnPassant = None
            plies = []
            pliesWithoutPawnOrCapture = 0
            repeatableStates = []
            numberOfMoves = 1
        }
        
    /// constructor
    let board (rep: ChessState) = rep.board
    let playerInTurn (rep: ChessState) = rep.playerInTurn
    let isCheck (rep: ChessState) = rep.isCheck
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

let newStandardChessGame = getOutcomeFromNewBoard ChessState.initial

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

let initialStandardChessPosition =
    ChessState.initial |> getOutcomeFromNewBoard

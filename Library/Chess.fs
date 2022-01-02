module Chess
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

type Reach = seq<File * Rank> list

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

    let bishopReaches coord: Reach =
        [ UpRight; DownRight; DownLeft; UpLeft ]
        |> List.map Seq.unfoldSimple
        |> List.apply coord

    let rookReaches coord: Reach =
        [ Up; Right; Down; Left ]
        |> List.map Seq.unfoldSimple
        |> List.apply coord

    let queenReaches coord: Reach =
        [ bishopReaches; rookReaches ]
        |> List.apply coord
        |> List.concat

    let kingReaches coord: Reach =
        [ UpRight; DownRight; DownLeft; UpLeft; Up; Right; Down; Left ]
        |> List.apply coord
        |> List.filterNones
        |> List.map Seq.singleton

    let knightReaches coord: Reach =
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
        |> List.apply coord
        |> List.filterNones
        |> List.map Seq.singleton

    let pawnSingleMoveReaches pawnDirection coord: Reach =
        match pawnDirection coord with
        | Some newCoord -> newCoord |> Seq.singleton |> List.singleton
        | None -> []

    let pawnDoubleMoveReaches pawnDirection coord: Reach =
        Seq.unfoldSimple pawnDirection coord
        |> Seq.take 2
        |> List.singleton

    let pawnCaptureReaches pawnDirection coord: Reach =
        [
            pawnDirection >?> Left;
            pawnDirection >?> Right;
        ]
        |> List.apply coord
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToKingReach castlingDirection coord: Reach =
        [ castlingDirection * 2; ]
        |> List.apply coord
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToQueenReach castlingDirection coord: Reach =
        [ castlingDirection * 3; ]
        |> List.apply coord
        |> List.filterNones
        |> List.map Seq.singleton

    type PawnRankType =
        | PromotionRank
        | EnPassantRank
        | DoubleMoveRank
        | RegularRank

    let pieceReaches piece coord =
        let applyReaches (reachesFunc, act: PlyType) =
            coord
            |> reachesFunc
            |> List.where (not << Seq.isEmpty)
            |> List.map (fun reach -> (reach, act))

        let collectReaches = List.collect applyReaches

        match piece with
        | Piece (color, Pawn) ->
            let _, rank = coord
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

    let pieceCoord = function
        | PieceSquare (_, coord) -> coord
        | EmptySquare coord -> coord

let squareAt coord placedPieces =
    let found = placedPieces |> Map.tryFind coord

    match found with
    | Some x -> PieceSquare (x, coord)
    | None -> EmptySquare coord

let (@@) placedPieces coord = squareAt coord placedPieces

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
            let coordOfCapturedPawn = (file target, rank source)
            RemovePiece coordOfCapturedPawn
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

let coordOfCapturedPiece = function
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

/// <summary>
/// </summary>
/// <remarks>
/// In chess, the state of the game is defined by several properties in addition
/// to the pieces on the board. We need to know the castling rights of each player,
/// whether the last move allows en passant capture (and the coordinate of the pawn that
/// can be captured), the repeated coordinates and their number, and the number of
/// moves since a pawn moved or a capture happened.
/// </remarks>
type ChessState = {
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

let internal (@@@) game coord = squareAt coord game.pieces

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
let internal canExecute game sourcePiece targetCoord plyType =
    let targetSquare = game @@@ targetCoord

    match plyType with
    | MoveType | MoveAndPromoteType ->
        targetSquare |> Square.isEmpty
    | CaptureType | CaptureAndPromoteType ->
        targetSquare |> Square.hasPiece &&
        targetSquare |> (differentColor sourcePiece)
    | EnPassantType ->
        match game.pawnCapturableEnPassant with
        | Some capturable -> capturable = targetCoord
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

let internal evaluateReachSquare game plyTypeToTest piece targetCoord =
    let canDoPlyType = canExecute game piece targetCoord plyTypeToTest
    let canMove = canExecute game piece targetCoord MoveType
    if canDoPlyType then Some (piece, plyTypeToTest, targetCoord)
    // Even if the ply type cannot be executed, if you can move to the
    // target coordinate we still return something, so that the ply type
    // is evaluated further in the same reach.
    elif canMove then Some (piece, MoveType, targetCoord)
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
let internal pieceCapabilitiesWithoutCheckFilter game piece sourceCoord =
    Reach.pieceReaches piece sourceCoord
    |> Seq.collect (fun (reach, plyType) -> reachCapabilities game piece plyType reach)
    |> Seq.collect (toPlies sourceCoord)

/// <summary>Coordinates attacked by a piece.</summary>
let internal attacksBy game (coord, piece) =
    pieceCapabilitiesWithoutCheckFilter game piece coord
    |> Seq.map coordOfCapturedPiece
    |> Seq.filterNones

/// <summary>Coordinates attacked by a player.</summary>
let internal attacks playerColor game =
    game.pieces
    |> Map.filter (fun _ (Piece (pieceColor, _)) -> pieceColor = playerColor)
    |> Map.toSeq
    |> Seq.collect (attacksBy game)

/// <summary>Is the coordinate attacked by a player?</summary>
let internal isAttackedBy playerColor game targetCoord =
    game
    |> attacks playerColor
    |> Seq.contains targetCoord

/// <summary>Executes a board change (unchecked).</summary>
let private rawBoardChange pieces boardChange =
    match boardChange with
    | MovePiece (sourceCoord, targetCoord) ->
        match pieces @@ sourceCoord with
        | EmptySquare _ ->
            failwith $"Trying to move a piece at %A{sourceCoord} but there is no piece there."
        | PieceSquare (piece, _) ->
            pieces
            |> Map.remove sourceCoord
            |> Map.remove targetCoord
            |> Map.add targetCoord piece
    | RemovePiece coord ->
        assert (pieces |> Map.containsKey coord)

        pieces
        |> Map.remove coord
    | AddPiece (col, shp, coord) ->
        pieces
        |> Map.remove coord
        |> Map.add coord (Piece (col, shp))

let opponent = opposite

/// <summary>Is the player in check?</summary>
let internal isCheck playerColor game =
    let possibleKingCoord = game.pieces |> Map.tryFindKey (fun _ piece -> piece = Piece (playerColor, King))
    match possibleKingCoord with
    | Some kingCoord -> isAttackedBy (opponent playerColor) game kingCoord
    | None -> false

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

let internal pieceCapabilities game (piece, sourceCoord) =
    let checkFilter ply =
        nextGameState ply [] false game
        |> (not << isCheck game.playerInTurn)

    pieceCapabilitiesWithoutCheckFilter game piece sourceCoord
    |> Seq.where checkFilter

type ChessStateRepresentation = {
    board: Square[,]
    playerInTurn: Player
    isCheck: bool
    whitePlayerCastlingRights: CastlingRights
    blackPlayerCastlingRights: CastlingRights
    pawnCapturableEnPassant: Coordinate option
    pliesWithoutPawnOrCapture: int
    moves: PlyOutput list
    numberOfMoves: int
}

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
    | GameStarted of ChessStateRepresentation * ExecutableAction list
    | PlayerMoved of ChessStateRepresentation * ExecutableAction list * bool
    | WonByCheckmate of ChessStateRepresentation * Color
    | LostByResignation of ChessStateRepresentation * Color
    | Draw of ChessStateRepresentation * Color * DrawType

and ExecutableAction = PlayerActionRepresentation * (unit -> PlayerActionOutcome)

and internal PlayerAction =
    | MovePiece of Ply * Ply list * bool
    | Resign
    | AcceptDraw

and PlayerActionRepresentation =
    | MovePiece of Ply * Ply list * bool
    | Resign
    | AcceptDraw

let internal toRepresentation (pa: PlayerAction) =
    match pa with
    | PlayerAction.MovePiece (ply, rest, drawOffered) -> PlayerActionRepresentation.MovePiece (ply, rest, drawOffered)
    | PlayerAction.Resign -> PlayerActionRepresentation.Resign
    | PlayerAction.AcceptDraw -> PlayerActionRepresentation.AcceptDraw

module PlayerActionOutcome =
    let representation = function
        | Draw (repr, _, _) -> repr
        | LostByResignation (repr, _) -> repr
        | WonByCheckmate (repr, _) -> repr
        | GameStarted (repr, _) -> repr
        | PlayerMoved (repr, _, _) -> repr

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

let representation (game: ChessState) =
    {
        playerInTurn = game.playerInTurn
        whitePlayerCastlingRights = game.whitePlayerCastlingRights
        blackPlayerCastlingRights = game.blackPlayerCastlingRights
        pawnCapturableEnPassant = game.pawnCapturableEnPassant
        board = Array2D.init 8  8 (fun r f ->
            game @@@ (File.fromInt f, Rank.fromInt r)
        )
        moves = List.rev game.plies
        pliesWithoutPawnOrCapture = game.pliesWithoutPawnOrCapture
        numberOfMoves = game.numberOfMoves
        isCheck = isCheck game.playerInTurn game
    }

// Possible plies for the player in turn.
let internal playerPlies gameState =
    let getCapabilities (sourceCoord, sourcePiece) =
        pieceCapabilities gameState (sourcePiece, sourceCoord)

    let belongsToPlayer _ (Piece (pieceColor, _)) = pieceColor = gameState.playerInTurn
    
    gameState.pieces
    |> Map.filter belongsToPlayer
    |> Map.toSeq
    |> Seq.collect getCapabilities

// given a player & a gameState, it returns a move result for that player.
let rec trol = 1

// player makes a move
and internal executePlayerAction (gameState: ChessState) (playerAction: PlayerAction): PlayerActionOutcome =
    match playerAction with
    | PlayerAction.MovePiece (ply, restOfPlies, drawOffer) ->
        let newGameState = nextGameState ply restOfPlies drawOffer gameState
        getOutcomeFromNewBoard newGameState drawOffer
    | PlayerAction.Resign ->
        let repr = representation gameState
        LostByResignation (repr, (opponent gameState.playerInTurn))
    | PlayerAction.AcceptDraw ->
        let repr = representation gameState
        Draw (repr, gameState.playerInTurn, Agreement)

and internal getOutcomeFromNewBoard (gameState: ChessState) drawOffer =
    let repr = representation gameState

    if drawOffer && gameState.pliesWithoutPawnOrCapture >= 50 then
        Draw (repr, gameState.playerInTurn, FiftyMovements)
    elif drawOffer &&  gameState.repetitionCount >= 3 then
        Draw (repr, gameState.playerInTurn, ThreefoldRepetition)
    elif gameState.pliesWithoutPawnOrCapture > 75 then
        Draw (repr, gameState.playerInTurn, SeventyFiveMovements)
    elif gameState.repetitionCount >= 5 then
        Draw (repr, gameState.playerInTurn, FivefoldRepetition)
    else
        let playerPlies = playerPlies gameState |> Seq.toList
        let canMove = playerPlies |> (not << List.isEmpty)
        if canMove then
            let actions = getExecutableActions playerPlies gameState drawOffer
            match gameState.plies |> List.length with
            | 0 -> GameStarted (repr, actions)
            | _ -> PlayerMoved (repr, actions, drawOffer)
        else if isCheck gameState.playerInTurn gameState then
            WonByCheckmate (repr, gameState.playerInTurn)
        else
            Draw (repr, gameState.playerInTurn, Stalemate)

// Convert possible plies to executable actions, adding Resign and OfferDraw
and internal getExecutableActions plies gameState drawOffer =
    let movePieces = plies |> Seq.collect (fun ply -> [PlayerAction.MovePiece (ply, plies, false); PlayerAction.MovePiece (ply, plies, true)])

    movePieces
    |> Seq.append [PlayerAction.Resign]
    |> Seq.append (if drawOffer then [PlayerAction.AcceptDraw] else [])
    |> Seq.map (makeNextExecutableAction gameState)
    |> Seq.toList

and internal makeNextExecutableAction gameState playerAction =
    let executeFn() = executePlayerAction gameState playerAction
    (toRepresentation playerAction), executeFn

let initialGameState =
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

let newStandardChessGame = getOutcomeFromNewBoard initialGameState false

let nullValidate = Ok

module Result =
    let ofOption errorData = function
        | Some x -> Ok x
        | None -> Error errorData

    let ofBool errorData = function
    | true -> Ok ()
    | false -> Error errorData

let (>=>) f1 f2 arg =
  match f1 arg with
  | Ok data -> f2 data
  | Error e -> Error e

let validateStandardChess (gameState: ChessState) =
    let invertedPieces (gameState: ChessState) =
        gameState.pieces
        |> Map.toList
        |> List.groupBy snd
        |> List.map (fun (k, pieces) -> (k, List.map (fun (coord, _) -> coord) pieces))
        |> Map.ofList

    let pieceCoords p = Map.tryFind p >> Result.ofOption $"No piece %A{p}"

    let length = List.length >> Some
    let thereIsOnlyPiece = List.length >> (fun x -> x = 1) >> Result.ofBool "There are more than one"


    let (?>>) f1 f2 = f1 >> Option.map f2
    let query f2 = function
    | Some x -> f2 x
    | None -> false

    let onlyOnePiece p =
        gameState |> (invertedPieces >> Map.tryFind p ?>> List.length >> query (fun x -> x = 1))

    let pieceCoordIs p coord =
        invertedPieces >> Map.tryFind p >> query (fun x -> List.contains coord x)

    let whiteCastlingKingside =
        (not gameState.whitePlayerCastlingRights.canCastleKingSide) ||
        (
        gameState |> (pieceCoordIs WhiteKing E1) &&
        gameState |> (pieceCoordIs WhiteRook H1))

    if (not <| onlyOnePiece WhiteKing) then
        Error "Not 1 white king"
    elif (not <| onlyOnePiece BlackKing) then
        Error "Not 1 black king"
    elif (not whiteCastlingKingside) then
        Error "White can castle kingside but the king is not at E1 or a rook at H1"
    else
        Ok gameState


let setupChessPosition gameState validate drawOffer =
    gameState
    |> validate
    |> Result.map (fun x -> getOutcomeFromNewBoard x drawOffer)

let setupStandardChessPosition gameState drawOffer =
    setupChessPosition gameState validateStandardChess drawOffer

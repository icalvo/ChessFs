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

    let bishopReaches pos: Reach =
        [ UpRight; DownRight; DownLeft; UpLeft ]
        |> List.map Seq.unfoldSimple
        |> List.apply pos

    let rookReaches pos: Reach =
        [ Up; Right; Down; Left ]
        |> List.map Seq.unfoldSimple
        |> List.apply pos

    let queenReaches pos: Reach =
        [ bishopReaches; rookReaches ]
        |> List.apply pos
        |> List.concat

    let kingReaches pos: Reach =
        [ UpRight; DownRight; DownLeft; UpLeft; Up; Right; Down; Left ]
        |> List.apply pos
        |> List.filterNones
        |> List.map Seq.singleton

    let knightReaches pos: Reach =
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
        |> List.apply pos
        |> List.filterNones
        |> List.map Seq.singleton

    let pawnSingleMoveReaches pawnDirection pos: Reach =
        match pawnDirection pos with
        | Some newPos -> newPos |> Seq.singleton |> List.singleton
        | None -> []

    let pawnDoubleMoveReaches pawnDirection pos: Reach =
        Seq.unfoldSimple pawnDirection pos
        |> Seq.take 2
        |> List.singleton

    let pawnCaptureReaches pawnDirection pos: Reach =
        [
            pawnDirection >?> Left;
            pawnDirection >?> Right;
        ]
        |> List.apply pos
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToKingReach castlingDirection (pos: Position): Reach =
        [ castlingDirection * 2; ]
        |> List.apply pos
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToQueenReach castlingDirection (pos: Position): Reach =
        [ castlingDirection * 3; ]
        |> List.apply pos
        |> List.filterNones
        |> List.map Seq.singleton

    type PawnRankType =
        | PromotionRank
        | EnPassantRank
        | DoubleMoveRank
        | RegularRank

    let pieceReaches piece pos =

        let applyReaches (reachesFunc, act: PlyType) =
            pos
            |> reachesFunc
            |> List.where (not << Seq.isEmpty)
            |> List.map (fun reach -> (reach, act))

        let collectReaches = List.collect applyReaches

        match piece with
        | Piece (color, Pawn) ->
            let _, rank = pos
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
    | PieceSquare of Piece * Position
    | EmptySquare of Position

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

    let piecePosition = function
        | PieceSquare (_, pos) -> pos
        | EmptySquare pos -> pos

let squareAt pos placedPieces =
    let found = placedPieces |> Map.tryFind pos

    match found with
    | Some x -> PieceSquare (x, pos)
    | None -> EmptySquare pos

let (@@) placedPieces pos = squareAt pos placedPieces

type BoardChange =
    | MovePiece of Position * Position
    | RemovePiece of Position
    | AddPiece of Color * Shape * Position

/// <summary>
/// A ply represents a piece move made by one of the players.
/// </summary>
type Ply =
    | Move of Piece * Position * Position
    | Capture of Piece * Position * Position
    | CaptureEnPassant of Color * Position * Position
    | CastleKingSide of Color
    | CastleQueenSide of Color
    | Promote of Color * Position * Position * Shape
    | CaptureAndPromote of Color * Position * Position * Shape
    member x.plyType =
        match x with
        | Move _ -> MoveType
        | Capture _ -> CaptureType
        | CaptureEnPassant _ -> EnPassantType
        | CastleKingSide _ -> CastleKingSideType
        | CastleQueenSide _ -> CastleQueenSideType
        | Promote _ -> MoveAndPromoteType
        | CaptureAndPromote _ -> CaptureAndPromoteType
    member x.isCapture =
        match x with
        | Capture _
        | CaptureAndPromote _
        | CaptureEnPassant _ -> true
        | _ -> false
    member x.source =
        match x with
        | Move (_, source, _) -> source
        | Capture (_, source, _) -> source
        | CaptureEnPassant (_, source, _) -> source
        | CastleKingSide White
        | CastleQueenSide White -> E1
        | CastleKingSide Black
        | CastleQueenSide Black -> E8
        | Promote (_, source, _, _) -> source
        | CaptureAndPromote (_, source, _, _) -> source
    member x.target =
        match x with
        | Move (_, _, target) -> target
        | Capture (_, _, target) -> target
        | CaptureEnPassant (_, _, target) -> target
        | CastleKingSide White -> G1
        | CastleQueenSide White -> C1
        | CastleKingSide Black -> G8
        | CastleQueenSide Black -> C8
        | Promote (_, _, target, _) -> target
        | CaptureAndPromote (_, _, target, _) -> target
    member x.shape =
        match x with
        | Move (Piece (_, shape), _, _) -> shape
        | Capture (Piece(_, shape), _, _) -> shape
        | CaptureEnPassant _ -> Pawn
        | CastleKingSide _ -> King
        | CastleQueenSide _ -> King
        | Promote _ -> Pawn
        | CaptureAndPromote _ -> Pawn
    member x.boardChanges =
        match x with
        | Move (_, source, target) -> [
                MovePiece (source, target)
            ]
        | Capture (_, source, target) -> [
            RemovePiece target;
            MovePiece (source, target)]
        | CaptureEnPassant (_, source, target) -> [
            let positionOfCapturedPawn = (file target, rank source)
            RemovePiece positionOfCapturedPawn
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

let positionOfCapturedPiece = function
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

type RepetitionState = {
    turn: Color
    whitePlayerCastlingRights: CastlingRights
    blackPlayerCastlingRights: CastlingRights
    pawnCapturableEnPassant: Position option
    pieces: Map<File * Rank, Piece>
}

/// <summary>
/// A ply produces an output that can be a check, a checkmate, or none (regular ply).
/// </summary>
type PlyOutput = {
    ply: Ply
    isCheck: bool
    restOfPlies: Ply list
}

let CheckPly (ply, restOfPlies) = { ply = ply; restOfPlies = restOfPlies; isCheck = true }
let RegularPly (ply, restOfPlies) = { ply = ply; restOfPlies = restOfPlies; isCheck = false }

/// <summary>
/// </summary>
/// <remarks>
/// In chess, the state of the game is defined by several properties in addition
/// to the pieces on the board. We need to know the castling rights of each player,
/// whether the last move allows en passant capture (and the position of the pawn that
/// can be captured), the repeated positions and their number, and the number of
/// moves since a pawn moved or a capture happened.
/// </remarks>
type internal ChessState = {
    playerInTurn: Player
    whitePlayerCastlingRights: CastlingRights
    blackPlayerCastlingRights: CastlingRights
    pawnCapturableEnPassant: Position option
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

let internal (@@@) game pos = squareAt pos game.pieces

/// <summary>
/// Generates plies from a ply type, a piece, and a source and target positions.
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
/// Can the ply type be legally executed for a piece with a given target position?
/// </summary>
let internal canExecute game sourcePiece targetPos plyType =
    let targetSquare = game @@@ targetPos

    match plyType with
    | MoveType | MoveAndPromoteType ->
        targetSquare |> Square.isEmpty
    | CaptureType | CaptureAndPromoteType ->
        targetSquare |> Square.hasPiece &&
        targetSquare |> (differentColor sourcePiece)
    | EnPassantType ->
        match game.pawnCapturableEnPassant with
        | Some capturable -> capturable = targetPos
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

let internal evaluateReachSquare game plyTypeToTest piece targetPos =
    let canDoPlyType = canExecute game piece targetPos plyTypeToTest
    let canMove = canExecute game piece targetPos MoveType
    if canDoPlyType then Some (piece, plyTypeToTest, targetPos)
    // Even if the ply type cannot be executed, if you can move to the
    // target position we still return something, so that the ply type
    // is evaluated further in the same reach.
    elif canMove then Some (piece, MoveType, targetPos)
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
let internal pieceCapabilitiesWithoutCheckFilter game piece sourcePos =
    Reach.pieceReaches piece sourcePos
    |> Seq.collect (fun (reach, plyType) -> reachCapabilities game piece plyType reach)
    |> Seq.collect (toPlies sourcePos)

/// <summary>Positions attacked by a piece.</summary>
let internal attacksBy game (position, piece) =
    pieceCapabilitiesWithoutCheckFilter game piece position
    |> Seq.map positionOfCapturedPiece
    |> Seq.filterNones

/// <summary>Positions attacked by a player.</summary>
let internal attacks playerColor game =
    game.pieces
    |> Map.filter (fun _ (Piece (pieceColor, _)) -> pieceColor = playerColor)
    |> Map.toSeq
    |> Seq.collect (attacksBy game)

/// <summary>Is the position attacked by a player?</summary>
let internal isAttackedBy playerColor game targetPosition =
    game
    |> attacks playerColor
    |> Seq.contains targetPosition

/// <summary>Executes a board change (unchecked).</summary>
let private rawBoardChange pieces boardChange =
    match boardChange with
    | MovePiece (sourcePos, targetPos) ->
        match pieces @@ sourcePos with
        | EmptySquare _ ->
            failwith $"Trying to move a piece at %A{sourcePos} but there is no piece there."
        | PieceSquare (piece, _) ->
            pieces
            |> Map.remove sourcePos
            |> Map.remove targetPos
            |> Map.add targetPos piece
    | RemovePiece pos ->
        assert (pieces |> Map.containsKey pos)

        pieces
        |> Map.remove pos
    | AddPiece (col, shp, pos) ->
        pieces
        |> Map.remove pos
        |> Map.add pos (Piece (col, shp))

let opponent = opposite

/// <summary>Is the player in check?</summary>
let internal isCheck playerColor game =
    let possibleKingPosition = game.pieces |> Map.tryFindKey (fun _ piece -> piece = Piece (playerColor, King))
    match possibleKingPosition with
    | Some kingPosition -> isAttackedBy (opponent playerColor) game kingPosition
    | None -> false

let private rawExecutePly (ply:Ply) (pieces:Map<Position, Piece>) =
    ply.boardChanges
    |> List.fold rawBoardChange pieces

let internal nextGameState (ply:Ply) (restOfPlies: Ply list) gameState =
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
    let hasStructureChanged = ply.isCapture || ply.shape = Pawn
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
    }
    { nextGameStateTemp with
        plies = newPlyOutput :: nextGameStateTemp.plies
    }

let internal pieceCapabilities game (piece, sourcePos) =
    let checkFilter ply =
        nextGameState ply [] game
        |> (not << isCheck game.playerInTurn)

    pieceCapabilitiesWithoutCheckFilter game piece sourcePos
    |> Seq.where checkFilter

type ChessStateRepresentation = {
    board: Square[,]
    playerInTurn: Player
    isCheck: bool
    whitePlayerCastlingRights: CastlingRights
    blackPlayerCastlingRights: CastlingRights
    pawnCapturableEnPassant: Position option
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
    | PlayerMoved of ChessStateRepresentation * ExecutableAction list
    | WonByCheckmate of ChessStateRepresentation * Color
    | LostByResignation of ChessStateRepresentation * Color
    | Draw of ChessStateRepresentation * Color * DrawType
    | DrawOffer of ChessStateRepresentation * Color * ExecutableAction list
    | DrawDeclinement of ChessStateRepresentation * Color * ExecutableAction list

and ExecutableAction = {
    action: PlayerActionDisplay
    execute: unit -> PlayerActionOutcome
}

and internal PlayerAction =
    | MovePiece of Ply * Ply list
    | Resign
    | OfferDraw of ExecutableAction list
    | AcceptDraw
    | DeclineDraw of ExecutableAction list

and PlayerActionDisplay =
    | MovePiece of Ply * Ply list
    | Resign
    | OfferDraw
    | AcceptDraw
    | DeclineDraw

let internal toDisplay (pa: PlayerAction) =
    match pa with
    | PlayerAction.MovePiece (ply, rest) -> PlayerActionDisplay.MovePiece (ply, rest)
    | PlayerAction.Resign -> PlayerActionDisplay.Resign
    | PlayerAction.OfferDraw _  -> PlayerActionDisplay.OfferDraw
    | PlayerAction.AcceptDraw -> PlayerActionDisplay.AcceptDraw
    | PlayerAction.DeclineDraw _ -> PlayerActionDisplay.DeclineDraw

type PlayerActionOutcome with
    member this.displayInfo =
        match this with
        | Draw (displayInfo, _, _) -> displayInfo
        | DrawDeclinement (displayInfo, _, _) -> displayInfo
        | LostByResignation (displayInfo, _) -> displayInfo
        | WonByCheckmate (displayInfo, _) -> displayInfo
        | GameStarted (displayInfo, _) -> displayInfo
        | PlayerMoved (displayInfo, _) -> displayInfo
        | DrawOffer (displayInfo, _, _) -> displayInfo

    member this.actions =
        match this with
        | Draw _
        | LostByResignation _
        | WonByCheckmate _ -> 
            []
        | GameStarted (_, availableActions)
        | PlayerMoved (_, availableActions)
        | DrawOffer (_, _, availableActions)
        | DrawDeclinement (_, _, availableActions) ->
            availableActions

let actions = function
    | Draw _
    | LostByResignation _
    | WonByCheckmate _ -> 
        []
    | GameStarted (_, availableActions)
    | PlayerMoved (_, availableActions)
    | DrawOffer (_, _, availableActions)
    | DrawDeclinement (_, _, availableActions) ->
        availableActions

let internal getDisplayInfo (game: ChessState) =
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
    let getCapabilities (sourcePos, sourcePiece) =
        pieceCapabilities gameState (sourcePiece, sourcePos)

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
    | PlayerAction.MovePiece (ply, restOfPlies) ->
        let newGameState = nextGameState ply restOfPlies gameState
        getOutcomeFromNewBoard newGameState
    | PlayerAction.Resign ->
        let displayInfo = getDisplayInfo gameState
        LostByResignation (displayInfo, (opponent gameState.playerInTurn))
    | PlayerAction.AcceptDraw ->
        let displayInfo = getDisplayInfo gameState
        Draw (displayInfo, gameState.playerInTurn, Agreement)
    | PlayerAction.DeclineDraw actions ->
        let newGameState = { gameState with playerInTurn = opposite gameState.playerInTurn }
        let displayInfo = getDisplayInfo newGameState
        DrawDeclinement (displayInfo, newGameState.playerInTurn, actions)
    | PlayerAction.OfferDraw actions ->
        if gameState.pliesWithoutPawnOrCapture >= 50 then
            let displayInfo = getDisplayInfo gameState
            Draw (displayInfo, gameState.playerInTurn, FiftyMovements)
        elif gameState.repetitionCount >= 3 then
            let displayInfo = getDisplayInfo gameState
            Draw (displayInfo, gameState.playerInTurn, ThreefoldRepetition)
        else
            let newGameState = { gameState with playerInTurn = opposite gameState.playerInTurn }
            let displayInfo = getDisplayInfo newGameState
            let drawOfferActions = getDrawOfferActions newGameState actions
            DrawOffer (displayInfo, newGameState.playerInTurn, drawOfferActions)

and internal getOutcomeFromNewBoard (gameState: ChessState) =
    if gameState.pliesWithoutPawnOrCapture > 75 then
        let displayInfo = getDisplayInfo gameState
        Draw (displayInfo, gameState.playerInTurn, SeventyFiveMovements)
    elif gameState.repetitionCount >= 5 then
        let displayInfo = getDisplayInfo gameState
        Draw (displayInfo, gameState.playerInTurn, FivefoldRepetition)
    else
        let playerPlies = playerPlies gameState |> Seq.toList
        let canMove = playerPlies |> (not << List.isEmpty)
        if canMove then
            let actions = getExecutableActions playerPlies gameState
            let representation = getDisplayInfo gameState
            match gameState.plies |> List.length with
            | 0 -> GameStarted (representation, actions)
            | _ -> PlayerMoved (representation, actions)
        else if isCheck gameState.playerInTurn gameState then
            let displayInfo = getDisplayInfo gameState
            WonByCheckmate (displayInfo, gameState.playerInTurn)
        else
            let displayInfo = getDisplayInfo gameState
            Draw (displayInfo, gameState.playerInTurn, Stalemate)

and internal getDrawOfferActions gameState actionsBeforeOffer =
    [PlayerAction.AcceptDraw;PlayerAction.DeclineDraw actionsBeforeOffer]
    |> Seq.map (makeNextExecutableAction gameState)
    |> Seq.toList

// Convert possible plies to executable actions, adding Resign and OfferDraw
and internal getExecutableActions plies gameState =
    let movePieces = plies |> Seq.map (fun ply -> PlayerAction.MovePiece (ply, plies))
    let execs = movePieces |> Seq.map (makeNextExecutableAction gameState) |> Seq.toList

    movePieces
    |> Seq.append [PlayerAction.Resign;PlayerAction.OfferDraw execs]
    |> Seq.map (makeNextExecutableAction gameState)
    |> Seq.toList

and internal makeNextExecutableAction gameState playerAction =
    // the capability has the player & action to take & gameState baked in
    let executeFn() = executePlayerAction gameState playerAction
    { action = toDisplay playerAction; execute = executeFn }

let internal initialGameState =
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

let newChessGame = getOutcomeFromNewBoard initialGameState
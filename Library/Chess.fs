module Chess
open Utils
open CoreTypes


type Piece = Piece of Color * Shape
let placedPiece color shape (pos: Position) = (Piece (color, shape), pos)

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
    let UpRight   = File.next >> Option.bind Rank.next
    let Right     = File.next
    let DownRight = File.next >> Option.bind Rank.prev
    let Down      = Rank.prev
    let DownLeft  = File.prev >> Option.bind Rank.prev
    let Left      = File.prev
    let UpLeft    = File.prev >> Option.bind Rank.next

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
        |> List.choose id
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
        |> List.choose id
        |> List.map Seq.singleton

    let pawnSingleMoveReaches pawnDirection pos: Reach =
        Seq.unfoldSimple pawnDirection pos
        |> Seq.take 1
        |> List.singleton

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

        let applyReaches (reachesfn, act: PlyType) =
            pos
            |> reachesfn
            |> List.filter (not << Seq.isEmpty)
            |> List.map (fun (reach) -> (reach, act))

        let collectReaches = List.collect applyReaches

        match piece with
        | Piece (color, Pawn) ->
            let (_, rank) = pos
            let direction = match color with | White -> Up | Black -> Down

            let pawnRankType =
                match (color, rank) with
                | (White, R7) | (Black, R2) -> PromotionRank
                | (White, R5) | (Black, R4) -> EnPassantRank
                | (White, R2) | (Black, R7) -> DoubleMoveRank
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
        | Piece (_, Knight) -> collectReaches [(knightReaches, MoveType);(knightReaches, CaptureType)]
        | Piece (_, Bishop) -> collectReaches [(bishopReaches, MoveType);(bishopReaches, CaptureType)]
        | Piece (_, Rook  ) -> collectReaches [(rookReaches, MoveType);(rookReaches, CaptureType)]
        | Piece (_, Queen ) -> collectReaches [(queenReaches, MoveType);(queenReaches, CaptureType)]
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

    let color = function
        | PieceSquare (Piece (col, _), _) -> Some col
        | _ -> None

    let piece = function
        | PieceSquare (piece, _) -> Some piece
        | _ -> None

    let position = function
        | PieceSquare (_, pos) -> pos
        | EmptySquare pos -> pos

let at pos placedPieces =
    placedPieces
    |> List.map PieceSquare
    |> List.tryFind (fun square -> Square.position square = pos)
    |> defaultArg <| EmptySquare pos

let (@@) placedPieces pos = at pos placedPieces

type Ply =
    | Move of Piece * Position * Position
    | Capture of Piece * Position * Position
    | CaptureEnPassant of Piece * Position * Position
    | CastleKingSide of Color
    | CastleQueenSide of Color
    | Promote of Piece * Position * Position * Shape
    | CaptureAndPromote of Piece * Position * Position * Shape

type BoardChange =
    | MovePiece of Position * Position
    | RemovePiece of Position
    | AddPiece of Color * Shape * Position

module Ply =
    let isCapture = function
        | Capture _
        | CaptureAndPromote _
        | CaptureEnPassant _ -> true
        | _ -> false
    let positions = function
        | Move (_, source, target) -> (source, target)
        | Capture (_, source, target) -> (source, target)
        | CaptureEnPassant (_, source, target) -> (source, target)
        | CastleKingSide White -> (E1, G1)
        | CastleQueenSide White -> (E1, C1)
        | CastleKingSide Black -> (E8, G8)
        | CastleQueenSide Black -> (E8, C8)
        | Promote (_, source, target, _) -> (source, target)
        | CaptureAndPromote (_, source, target, _) -> (source, target)
    let shape = function
        | Move (Piece (_, shape), _, _) -> shape
        | Capture (Piece(_, shape), _, _) -> shape
        | CaptureEnPassant _ -> Pawn
        | CastleKingSide _ -> King
        | CastleQueenSide _ -> King
        | Promote _ -> Pawn
        | CaptureAndPromote _ -> Pawn
    let boardChanges = function
        | Move (_, source, target) -> [MovePiece (source, target)]
        | Capture (_, source, target) -> [MovePiece (source, target)]
        | CaptureEnPassant (_, source, target) -> [MovePiece (source, target)]
        | CastleKingSide White -> [MovePiece (E1, G1); MovePiece (H1, F1)]
        | CastleQueenSide White -> [MovePiece (E1, C1); MovePiece (A1, D1)]
        | CastleKingSide Black -> [MovePiece (E8, G8); MovePiece (H8, F8)]
        | CastleQueenSide Black -> [MovePiece (E8, C8); MovePiece (A8, D8)]
        | Promote (Piece (color, _), source, target, shape) -> [RemovePiece source; AddPiece (color, shape, target)]
        | CaptureAndPromote (Piece (color, _), source, target, shape) -> [RemovePiece source; AddPiece (color, shape, target)]

    let captureTarget = function
        | Capture (_, _, target) -> Some target
        | CaptureAndPromote (_, _, target, _) -> Some target
        | _ -> None

let sameColor (Piece (sourceColor, _)) square =
    match Square.color square with
    | Some col -> col = sourceColor
    | None -> false
    
let differentColor p s = not (sameColor p s)

type CastleStatus = {
    canCastleKingside: bool
    canCastleQueenside: bool
}

let canCastle = { canCastleKingside = true; canCastleQueenside = true }

type GameState = {
    turn: Color
    whitePlayerCastleState: CastleStatus
    blackPlayerCastleState: CastleStatus
    pawnCapturableEnPassant: Position option
    pieces: (Piece * Position) list
    plies: Ply list
    pliesWithoutPawnOrCapture: int
    repeatableStates: GameState list
    numberOfMoves: int
}
with
    member this.currentPlayerCastleState = 
        match this.turn with
        | White -> this.whitePlayerCastleState
        | Black -> this.blackPlayerCastleState
    member this.repeated =
        this.repeatableStates
        |> List.filter (fun r -> r.pieces = this.pieces)
        |> List.length

let (@@@) game pos = at pos game.pieces

let toPly (Piece (color, shape), sourcePosition, plyType, targetPosition) =
    let p = Piece (color, shape)
    
    match plyType with
    | MoveType -> Move (p, sourcePosition, targetPosition)
    | CaptureType -> Capture (p, sourcePosition, targetPosition)
    | EnPassantType -> CaptureEnPassant (p, sourcePosition, targetPosition)
    | CastleKingSideType -> CastleKingSide color
    | CastleQueenSideType -> CastleQueenSide color
    | MoveAndPromoteType -> Promote (p, sourcePosition, targetPosition, shape)
    | CaptureAndPromoteType -> CaptureAndPromote (p, sourcePosition, targetPosition, shape)

let toPlies (Piece (color, sh), s, a, t) =
    let p = Piece (color, sh)
    match a with
    | MoveType -> [ Move (p, s, t) ]
    | CaptureType -> [ Capture (p, s, t) ]
    | EnPassantType -> [ CaptureEnPassant (p, s, t) ]
    | CastleKingSideType -> [ CastleKingSide color ]
    | CastleQueenSideType -> [ CastleQueenSide color ]
    | MoveAndPromoteType -> [ Queen; Rook; Bishop; Knight ] |> List.map (fun shape -> Promote (p, s, t, shape))
    | CaptureAndPromoteType -> [ Queen; Rook; Bishop; Knight ] |> List.map (fun shape -> CaptureAndPromote (p, s, t, shape))

let canExecute game sourcePiece sourcePos targetPos plyType =
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
            game.whitePlayerCastleState.canCastleKingside &&
            game @@@ F1 |> Square.isEmpty &&
            game @@@ G1 |> Square.isEmpty
        | Piece (Black, King) ->
            game.blackPlayerCastleState.canCastleKingside &&
            game @@@ F8 |> Square.isEmpty &&
            game @@@ G8 |> Square.isEmpty
        | _ -> false
    | CastleQueenSideType ->
        match sourcePiece with
        | Piece (White, King) ->
            game.whitePlayerCastleState.canCastleQueenside &&
            game @@@ B1 |> Square.isEmpty &&
            game @@@ C1 |> Square.isEmpty &&
            game @@@ D1 |> Square.isEmpty
        | Piece (Black, King) ->
            game.blackPlayerCastleState.canCastleQueenside &&
            game @@@ B8 |> Square.isEmpty &&
            game @@@ C8 |> Square.isEmpty &&
            game @@@ D8 |> Square.isEmpty
        | _ -> false

let evaluateReachSquare game plyTypeToTest sourcePiece sourcePos targetPos =
    let canDoPlyType = canExecute game sourcePiece sourcePos targetPos plyTypeToTest
    let canMove = canExecute game sourcePiece sourcePos targetPos MoveType
    if canDoPlyType then Some (sourcePiece, sourcePos, plyTypeToTest, targetPos)
    // If you can move we still return something so that the reach seq is further explored
    elif canMove then Some (sourcePiece, sourcePos, MoveType, targetPos)
    else None

let reachCapabilities game piece sourcePos (plyTypeToAnalyze: PlyType) reach =
    let actionIs plyTypeToCheck = function
        | Some (_, _, plyType, _) -> plyType = plyTypeToCheck
        | _ -> false

    reach
    |> Seq.map (evaluateReachSquare game plyTypeToAnalyze piece sourcePos)
    |> Seq.takeWhilePlusOne (actionIs MoveType)
    |> Seq.filter (actionIs plyTypeToAnalyze)
    |> Seq.filterNones

let pieceCapabilitiesWithoutCheckFilter game piece sourcePos =
    Reach.pieceReaches piece sourcePos
    |> Seq.collect (fun (reach, plyType) -> reachCapabilities game piece sourcePos plyType reach)
    |> Seq.collect toPlies

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacksBy game (piece, position) =
    pieceCapabilitiesWithoutCheckFilter game piece position
    |> Seq.map Ply.captureTarget
    |> Seq.filterNones

/// <summary>Determines the attacks a player is making.</summary>
let attacks col game =
    game.pieces
    |> Seq.filter (fun (Piece (pieceColor, _), _) -> pieceColor = col)
    |> Seq.collect (attacksBy game)

let isAttackedBy attackingColor game targetPosition =
    game
    |> attacks attackingColor
    |> Seq.contains targetPosition

let private rawBoardChange pieces boardChange =
    match boardChange with
    | BoardChange.MovePiece (sourcePos, targetPos) ->
        match pieces @@ sourcePos with
        | EmptySquare _ -> pieces |> List.filter (fun (_, position) -> position <> targetPos)
        | PieceSquare (piece, _) -> (piece, targetPos) :: (pieces |> List.filter (fun (_, position) -> position <> sourcePos && position <> targetPos))
    | BoardChange.RemovePiece pos -> pieces |> List.filter (fun (_, position) -> position <> pos)
    | BoardChange.AddPiece (col, shp, pos) ->
        (Piece (col, shp), pos) :: (pieces |> List.filter (fun (_, position) -> position <> pos))

let isCheck color game =
    game.pieces
    |> Seq.tryFind (fun (piece, _) -> piece = Piece (color, King))
    |> Option.map (fun (_, pos) -> pos)
    |> Option.map (isAttackedBy (opposite color) game)
    |> defaultArg <| false

let opponent = opposite

let rawExecutePly (ply:Ply) (pieces:(Piece*Position) list) =
    Ply.boardChanges ply
    |> List.fold rawBoardChange pieces

let nextGameState (ply:Ply) gameState =
    let otherPlayer = opponent gameState.turn
    let plyPreventsWhiteCastleKingside =
        match ply with
        | Move    (Piece (White, King), _, _)
        | Move    (Piece (White, Rook), (H, R1), _)
        | Capture (Piece (White, Rook), _, _)
        | Capture (Piece (White, King), _, _)
        | Capture (_, _, (H, R1)) -> true
        | _ -> false
    let plyPreventsBlackCastleKingside =
        match ply with
        | Move    (Piece (Black, King), _, _)
        | Move    (Piece (Black, Rook), (H, R8), _)
        | Capture (Piece (Black, Rook), _, _)
        | Capture (Piece (Black, King), _, _)
        | Capture (_, _, (H, R8)) -> true
        | _ -> false
    let plyPreventsWhiteCastleQueenside =
        match ply with
        | Move    (Piece (White, King), _, _)
        | Move    (Piece (White, Rook), (A, R1), _)
        | Capture (Piece (White, Rook), _, _)
        | Capture (Piece (White, King), _, _)
        | Capture (_, _, (A, R1)) -> true
        | _ -> false
    let plyPreventsBlackCastleQueenside =
        match ply with
        | Move    (Piece (Black, King), _, _)
        | Move    (Piece (Black, Rook), (A, R8), _)
        | Capture (Piece (Black, Rook), _, _)
        | Capture (Piece (Black, King), _, _)
        | Capture (_, _, (A, R8)) -> true
        | _ -> false
    let hasStructureChanged = Ply.isCapture ply || Ply.shape ply = Pawn

    {
        turn = otherPlayer
        pieces = gameState.pieces |> rawExecutePly ply
        pawnCapturableEnPassant =
            match ply with
            | Move (Piece (White, Pawn), (_, R2), (f, R4)) -> Some (f, R3)
            | Move (Piece (Black, Pawn), (_, R7), (f, R5)) -> Some (f, R6)
            | _ -> None
        blackPlayerCastleState =
            {
                canCastleKingside =
                    gameState.blackPlayerCastleState.canCastleKingside &&
                    not plyPreventsBlackCastleKingside
                canCastleQueenside =
                    gameState.blackPlayerCastleState.canCastleQueenside &&
                    not plyPreventsBlackCastleQueenside
            }
        whitePlayerCastleState =
            {
                canCastleKingside =
                    gameState.whitePlayerCastleState.canCastleKingside &&
                    not plyPreventsWhiteCastleKingside
                canCastleQueenside =
                    gameState.whitePlayerCastleState.canCastleQueenside &&
                    not plyPreventsWhiteCastleQueenside
            }
        plies = ply::gameState.plies
        pliesWithoutPawnOrCapture = if hasStructureChanged then 0 else gameState.pliesWithoutPawnOrCapture + 1
        repeatableStates = if hasStructureChanged then [] else gameState::gameState.repeatableStates
        numberOfMoves =
            match gameState.turn with
            | White -> gameState.numberOfMoves
            | Black -> gameState.numberOfMoves + 1
    }

let pieceCapabilities game (piece, sourcePos) =
    let checkFilter ply =
        nextGameState ply game
        |> (not << isCheck game.turn)

    pieceCapabilitiesWithoutCheckFilter game piece sourcePos
    |> Seq.filter checkFilter

type DisplayInfo = {
    board: Square[,]
    playerToMove: Color option
    isCheck: bool
    canCastleKingside: bool
    canCastleQueenside: bool
    moves: Ply list
    gameState: GameState
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
    | PlayerMoved of DisplayInfo * ExecutableAction list
    | WonByCheckmate of DisplayInfo * Color
    | LostByResignation of DisplayInfo * Color
    | Draw of DisplayInfo * Color * DrawType
    | DrawOffer of DisplayInfo * Color * ExecutableAction list
    | DrawDeclinement of DisplayInfo * Color * ExecutableAction list

/// <summary>
/// Information about one possible movement. It includes
/// the source and target position, and a function that, when
/// called, will return the result of the movement (a MoveResult).
/// </summary>
and ExecutableAction = {
    action: PlayerAction
    execute: unit -> PlayerActionOutcome
}

and PlayerAction =
    | MovePiece of Ply
    | Resign
    | OfferDraw of ExecutableAction list
    | AcceptDraw
    | DeclineDraw of ExecutableAction list

type PlayerActionOutcome with
    member this.displayInfo =
        match this with
        | Draw (displayInfo, _, _) -> displayInfo
        | DrawDeclinement (displayInfo, _, _) -> displayInfo
        | LostByResignation (displayInfo, _) -> displayInfo
        | WonByCheckmate (displayInfo, _) -> displayInfo
        | PlayerMoved (displayInfo, _) -> displayInfo
        | DrawOffer (displayInfo, _, _) -> displayInfo

let getFinalDisplayInfo game =
    {
        board = Array2D.init 8 8 (fun r f ->
            game @@@ (File.fromInt f, Rank.fromInt r)
        )
        playerToMove = None
        isCheck = isCheck game.turn game
        canCastleKingside = game.currentPlayerCastleState.canCastleKingside
        canCastleQueenside = game.currentPlayerCastleState.canCastleQueenside
        moves = List.rev game.plies
        gameState = game
    }

let getDisplayInfo game =
    {
        board = Array2D.init 8  8 (fun r f ->
            game @@@ (File.fromInt f, Rank.fromInt r)
        )
        playerToMove = Some game.turn
        isCheck = isCheck game.turn game
        canCastleKingside = game.currentPlayerCastleState.canCastleKingside
        canCastleQueenside = game.currentPlayerCastleState.canCastleQueenside
        moves = List.rev game.plies
        gameState = game
    }

// given a player & a gameState, it returns the possible player actions.
let playerPlies gameState =
    let getCapabilities (sourcePiece, sourcePos) =
        pieceCapabilities gameState (sourcePiece, sourcePos)

    let belongsToPlayer (Piece (pieceColor, _), _) = pieceColor = gameState.turn
    
    gameState.pieces
    |> Seq.filter belongsToPlayer
    |> Seq.collect getCapabilities

// given a player & a gameState, it returns a move result for that player.
let rec makePlayerMoveResultWithCapabilities gameState =
    let displayInfo = getDisplayInfo gameState
    let playerPlies = playerPlies gameState
    let canMove = playerPlies |> (not << Seq.isEmpty)
    if canMove then
        let playerMovementCapabilities =
            getExecutableActions playerPlies gameState
    
        PlayerMoved (displayInfo, playerMovementCapabilities)
    else
        let displayInfo = getFinalDisplayInfo gameState
        WonByCheckmate (displayInfo, gameState.turn)

and getDrawOfferActions gameState actionsBeforeOffer =
    [AcceptDraw;DeclineDraw actionsBeforeOffer]
    |> Seq.map (makeNextMoveInfo gameState)
    |> Seq.toList
// Convert possible plies to executable actions, adding Resign and OfferDraw
and getExecutableActions plies gameState =
    let movePieces = plies |> Seq.map MovePiece
    let execs = movePieces |> Seq.map (makeNextMoveInfo gameState) |> Seq.toList

    movePieces
    |> Seq.append [Resign;OfferDraw execs]
    |> Seq.map (makeNextMoveInfo gameState)
    |> Seq.toList

and makeNextMoveInfo gameState playerAction =
    // the capability has the player & action to take & gameState baked in
    let executeFn() = executePlayerAction gameState playerAction
    { action = playerAction; execute = executeFn }

// player makes a move
and executePlayerAction gameState playerAction: PlayerActionOutcome =
    match playerAction with
    | MovePiece ply ->
        let newGameState = nextGameState ply gameState
        if gameState.pliesWithoutPawnOrCapture >= 75 then
            let displayInfo = getFinalDisplayInfo newGameState
            Draw (displayInfo, gameState.turn, SeventyFiveMovements)
        elif (newGameState.repeatableStates |> List.filter (fun r -> r.pieces = newGameState.pieces) |> List.length) >= 5 then
            let displayInfo = getFinalDisplayInfo newGameState
            Draw (displayInfo, gameState.turn, FivefoldRepetition)
        else
            makePlayerMoveResultWithCapabilities newGameState
    | Resign ->
        let displayInfo = getFinalDisplayInfo gameState
        LostByResignation (displayInfo, (opponent gameState.turn))
    | AcceptDraw ->
        let displayInfo = getFinalDisplayInfo gameState
        Draw (displayInfo, gameState.turn, Agreement)
    | DeclineDraw actions ->
        let newGameState = { gameState with turn = opposite gameState.turn }
        let displayInfo = getDisplayInfo newGameState
        DrawDeclinement (displayInfo, newGameState.turn, actions)
    | OfferDraw actions ->
        if gameState.pliesWithoutPawnOrCapture >= 50 then
            let displayInfo = getFinalDisplayInfo gameState
            Draw (displayInfo, gameState.turn, FiftyMovements)
        elif (gameState.repeatableStates |> List.filter (fun r -> r.pieces = gameState.pieces) |> List.length) >= 3 then
            let displayInfo = getFinalDisplayInfo gameState
            Draw (displayInfo, gameState.turn, ThreefoldRepetition)
        else
            let newGameState = { gameState with turn = opposite gameState.turn }
            let displayInfo = getDisplayInfo newGameState
            let drawOfferActions = getDrawOfferActions newGameState actions
            DrawOffer (displayInfo, newGameState.turn, drawOfferActions)

let initialGameState =
    let initialPieces = [
        placedPiece White Rook   A1
        placedPiece White Knight B1
        placedPiece White Bishop C1
        placedPiece White Queen  D1
        placedPiece White King   E1
        placedPiece White Bishop F1
        placedPiece White Knight G1
        placedPiece White Rook   H1
        placedPiece White Pawn   A2
        placedPiece White Pawn   B2
        placedPiece White Pawn   C2
        placedPiece White Pawn   D2
        placedPiece White Pawn   E2
        placedPiece White Pawn   F2
        placedPiece White Pawn   G2
        placedPiece White Pawn   H2
        placedPiece Black Pawn   A7
        placedPiece Black Pawn   B7
        placedPiece Black Pawn   C7
        placedPiece Black Pawn   D7
        placedPiece Black Pawn   E7
        placedPiece Black Pawn   F7
        placedPiece Black Pawn   G7
        placedPiece Black Pawn   H7
        placedPiece Black Rook   A8
        placedPiece Black Knight B8
        placedPiece Black Bishop C8
        placedPiece Black Queen  D8
        placedPiece Black King   E8
        placedPiece Black Bishop F8
        placedPiece Black Knight G8
        placedPiece Black Rook   H8
    ]

    let tempGameState =
        {
            turn = White
            pieces = initialPieces
            whitePlayerCastleState = {
                                        canCastleKingside = true
                                        canCastleQueenside = true
                                     }
            blackPlayerCastleState = {
                                        canCastleKingside = true
                                        canCastleQueenside = true
                                     }
            pawnCapturableEnPassant = None
            plies = []
            pliesWithoutPawnOrCapture = 0
            repeatableStates = []
            numberOfMoves = 1
        }

    { tempGameState with repeatableStates = [ tempGameState ]}

let newChessGame = makePlayerMoveResultWithCapabilities initialGameState
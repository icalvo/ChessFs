module Chess
open Utils
open CoreTypes

// Directions
let Up        = nextRank
let UpRight   = nextFile >> Option.bind nextRank
let Right     = nextFile
let DownRight = nextFile >> Option.bind prevRank
let Down      = prevRank
let DownLeft  = prevFile >> Option.bind prevRank
let Left      = prevFile
let UpLeft    = prevFile >> Option.bind nextRank

type Reach = seq<File * Rank> list

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

let opposite = function
    | White -> Black
    | Black -> White

type Piece = Piece of Color * Shape

let WhitePawn = Piece (White, Pawn)
let BlackPawn = Piece (Black, Pawn)
let BlackKnight = Piece (Black, Knight)

type Square = 
    | PieceSquare of Piece * Position
    | EmptySquare of Position

let placedPiece color shape pos = (Piece (color, shape), pos)

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
    |> List.tryFind (fun square -> position square = pos)
    |> defaultArg <| EmptySquare pos

let (@) game pos = at pos game

type PlyType =
    | MoveType
    | CaptureType
    | EnPassantType
    | CastleKingSideType
    | CastleQueenSideType
    | MoveAndPromoteType
    | CaptureAndPromoteType

type Ply =
    | Move of Piece * Position * Position
    | Capture of Piece * Position * Position
    | EnPassant of Piece * Position * Position
    | CastleKingSide of Color
    | CastleQueenSide of Color
    | Promote of Piece * Position * Position * Shape
    | CaptureAndPromote of Piece * Position * Position * Shape

let plyPositions = function
    | Move (_, source, target) -> (source, target)
    | Capture (_, source, target) -> (source, target)
    | EnPassant (_, source, target) -> (source, target)
    | CastleKingSide White -> (E1, G1)
    | CastleQueenSide White -> (E1, C1)
    | CastleKingSide Black -> (E8, G8)
    | CastleQueenSide Black -> (E8, C8)
    | Promote (_, source, target, _) -> (source, target)
    | CaptureAndPromote (_, source, target, _) -> (source, target)

let plyBoardChanges = function
    | Move (_, source, target) -> [(source, target)]
    | Capture (_, source, target) -> [(source, target)]
    | EnPassant (_, source, target) -> [(source, target)]
    | CastleKingSide White -> [(E1, G1);(H1, F1)]
    | CastleQueenSide White -> [(E1, C1);(A1, D1)]
    | CastleKingSide Black -> [(E8, G8);(H8, F8)]
    | CastleQueenSide Black -> [(E8, C8);(A8, D8)]
    | Promote (_, source, target, _) -> [(source, target)]
    | CaptureAndPromote (_, source, target, _) -> [(source, target)]


let plyCaptureTarget = function
    | Capture (_, _, target) -> Some target
    | CaptureAndPromote (_, _, target, _) -> Some target
    | _ -> None


type PlayerAction =
    | MovePiece of Ply
    | Resign
    | OfferDraw

let pieceReaches piece pos =

    let applyReaches (reachesfn, act: PlyType) =
        pos
        |> reachesfn
        |> List.filter (not << Seq.isEmpty)
        |> List.map (fun (reach) -> (reach, act))

    let collectReaches = List.collect applyReaches

    match piece with
    | Piece (color, Pawn) ->
        let promotionRank = match color with | White -> R7 | Black -> R2
        let enPassantRank = match color with | White -> R5 | Black -> R4
        let doubleAdvRank = match color with | White -> R2 | Black -> R7
        let direction     = match color with | White -> Up | Black -> Down
        if (snd pos) = promotionRank then
            collectReaches [
                (pawnSingleMoveReaches direction, MoveAndPromoteType);
                (pawnCaptureReaches    direction, CaptureAndPromoteType)]
        elif (snd pos) = enPassantRank then
            collectReaches [
                (pawnSingleMoveReaches direction, MoveType);
                (pawnCaptureReaches    direction, CaptureType);
                (pawnCaptureReaches    direction, EnPassantType)]
        elif (snd pos) = doubleAdvRank then
            collectReaches [
                (pawnDoubleMoveReaches direction, MoveType   );
                (pawnCaptureReaches    direction, CaptureType);
            ]
        else
            collectReaches [
                (pawnSingleMoveReaches direction, MoveType   );
                (pawnCaptureReaches    direction, CaptureType);
            ]
    | Piece (_, Knight) -> collectReaches [(knightReaches, MoveType);(knightReaches, CaptureType)]
    | Piece (_, Bishop) -> collectReaches [(bishopReaches, MoveType);(bishopReaches, CaptureType)]
    | Piece (_, Rook  ) -> collectReaches [(rookReaches, MoveType);(bishopReaches, CaptureType)]
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

let sameColor (Piece (sourceColor, _)) square =
    match color square with
    | Some col -> col = sourceColor
    | None -> false
    
let differentColor p s = not (sameColor p s)

type CastleStatus = {
    canCastleKingside: bool
    canCastleQueenside: bool
}

let cannotCastle = { canCastleKingside = true; canCastleQueenside = true }

type GameState = {
    turn: Color
    whitePlayerCastleState: CastleStatus
    blackPlayerCastleState: CastleStatus
    enPassantPawn: Position option
    pieces: (Piece * Position) list
}


let canExecute game sourcePiece sourcePos targetPos plyType =
    let targetSquare = game.pieces @ targetPos
    match plyType with
    | MoveType | MoveAndPromoteType ->
        targetSquare |> isEmpty
    | CaptureType | CaptureAndPromoteType ->
        targetSquare |> hasPiece &&
        targetSquare |> (differentColor sourcePiece)
    | EnPassantType ->
        let passedPawnPos = (fst targetPos, snd sourcePos)

        // targetSquare |> isEmpty && // THIS SHOULD BE ALWAYS TRUE
        game.enPassantPawn
        |> Option.map (fun p -> p = passedPawnPos)
        |> Option.isSome
    | CastleKingSideType ->
        match sourcePiece with
        | Piece (White, King) -> game.whitePlayerCastleState.canCastleKingside
        | Piece (Black, King) -> game.blackPlayerCastleState.canCastleKingside
        | _ -> false
    | CastleQueenSideType ->
        match sourcePiece with
        | Piece (White, King) -> game.whitePlayerCastleState.canCastleQueenside
        | Piece (Black, King) -> game.blackPlayerCastleState.canCastleQueenside
        | _ -> false


let toPly (Piece (color, shape), sourcePosition, plyType, targetPosition) =
    let p = Piece (color, shape)
    
    match plyType with
    | MoveType -> Move (p, sourcePosition, targetPosition)
    | CaptureType -> Capture (p, sourcePosition, targetPosition)
    | EnPassantType -> EnPassant (p, sourcePosition, targetPosition)
    | CastleKingSideType -> CastleKingSide color
    | CastleQueenSideType -> CastleQueenSide color
    | MoveAndPromoteType -> Promote (p, sourcePosition, targetPosition, shape)
    | CaptureAndPromoteType -> CaptureAndPromote (p, sourcePosition, targetPosition, shape)

let toPlies (Piece (color, sh), s, a, t) =
    let p = Piece (color, sh)
    match a with
    | MoveType -> [ Move (p, s, t) ]
    | CaptureType -> [ Capture (p, s, t) ]
    | EnPassantType -> [ EnPassant (p, s, t) ]
    | CastleKingSideType -> [ CastleKingSide color ]
    | CastleQueenSideType -> [ CastleQueenSide color ]
    | MoveAndPromoteType -> [ Queen; Rook; Bishop; Knight ] |> List.map (fun shape -> Promote (p, s, t, shape))
    | CaptureAndPromoteType -> [ Queen; Rook; Bishop; Knight ] |> List.map (fun shape -> CaptureAndPromote (p, s, t, shape))

let evaluateSquare game actionToAnalyze sourcePiece sourcePos targetPos =
    let can = canExecute game sourcePiece sourcePos targetPos actionToAnalyze
    let canMove = canExecute game sourcePiece sourcePos targetPos MoveType
    if can then Some (sourcePiece, sourcePos, actionToAnalyze, targetPos)
    elif canMove then Some (sourcePiece, sourcePos, MoveType, targetPos)
    else None

let reachCapabilities game piece sourcePos (plyTypeToAnalyze: PlyType) reach =
    let actionIs (actionType:PlyType) = function
        | Some (_, _, act, _) -> act = actionType
        | _ -> false

    reach
    |> Seq.map (evaluateSquare game plyTypeToAnalyze piece sourcePos)
    |> Seq.takeWhilePlusOne (actionIs MoveType)
    |> Seq.filter (actionIs plyTypeToAnalyze)
    |> Seq.filterNones

let pieceCapabilitiesWithoutCheckFilter game piece sourcePos: seq<Ply> =
    let temp =
        pieceReaches piece sourcePos
        |> Seq.collect (fun (r, a) -> reachCapabilities game piece sourcePos a r)

    temp |> Seq.collect toPlies

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacksBy game (piece, position) =
    pieceCapabilitiesWithoutCheckFilter game piece position
    |> Seq.map plyCaptureTarget
    |> Seq.filterNones

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacks col game = 
    game.pieces
    |> Seq.filter (fun (Piece (pieceColor, _), _) -> pieceColor = col)
    |> Seq.collect (attacksBy game)

let isAttackedBy attackingColor game targetPosition =
    game
    |> attacks attackingColor
    |> Seq.contains targetPosition

/// <summary>Basic move operation. It simply empties the source pos and sets the target pos with the
/// same the source had before.</summary>
let private rawMoveAndReplace (sourcePos, targetPos) pieces =
    match pieces @ sourcePos with
    | EmptySquare _ -> pieces |> List.filter (fun (_, position) -> position <> targetPos)
    | PieceSquare (piece, _) -> (piece, targetPos) :: (pieces |> List.filter (fun (_, position) -> position <> sourcePos && position <> targetPos))

let isCheck color game =
    game.pieces
    |> Seq.tryFind (fun (piece, _) -> piece = Piece (color, King))
    |> Option.map (fun (_, pos) -> pos)
    |> Option.map (isAttackedBy (opposite color) game)
    |> defaultArg <| false

let opponent = opposite

let rawExecutePly (ply:Ply) pieces =
    rawMoveAndReplace (plyPositions ply) pieces

let nextGameState (ply:Ply) gameState =
    let otherPlayer = opponent gameState.turn
    { gameState with
        turn = otherPlayer
        pieces = gameState.pieces |> rawExecutePly ply
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
}

type DrawTypes =
    | Agreed
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
    | Draw of DisplayInfo * Color
    | DrawOffer of DisplayInfo * Color * ExecutableAction list

/// <summary>
/// Information about one possible movement. It includes
/// the source and target position, and a function that, when
/// called, will return the result of the movement (a MoveResult).
/// </summary>
and ExecutableAction = {
    action: PlayerAction
    execute: unit -> PlayerActionOutcome
}

let getFinalDisplayInfo game =
    {
        board = Array2D.init 8  8 (fun r f ->
            game.pieces @ (File.fromInt f, Rank.fromInt r)
        )
        playerToMove = None
        isCheck = isCheck game.turn game
    }

let getDisplayInfo game =
    {
        board = Array2D.init 8  8 (fun r f ->
            game.pieces @ (File.fromInt f, Rank.fromInt r)
        )
        playerToMove = Some game.turn
        isCheck = isCheck game.turn game
    }

let private isCheckmateTo player gameState =
    // TODO:
    // let (<&>) f g = (fun x -> f x && g x)
    // isCheck <&> capabilities 
    false

let private isGameTied player gameState = false

// given a player & a gameState, it returns the possible player actions.
let playerActions gameState =
    let getCapabilities (sourcePiece, sourcePos) =
        pieceCapabilities gameState (sourcePiece, sourcePos)
        |> Seq.map MovePiece

    let belongsToPlayer (Piece (pieceColor, _), _) = pieceColor = gameState.turn
    
    gameState.pieces
    |> Seq.filter belongsToPlayer
    |> Seq.collect getCapabilities
    |> Seq.append [Resign; OfferDraw]

// given a player & a gameState, it returns a move result for that player.
let rec makePlayerMoveResultWithCapabilities gameState =
    let displayInfo = getDisplayInfo gameState
    let playerActions = playerActions gameState
    let actionIsMove = function
        | MovePiece _ -> true
        | _ -> false

    let canMove = playerActions |> Seq.exists actionIsMove
    if canMove then
        let playerMovementCapabilities =
            playerActions
            |> Seq.map (makeNextMoveInfo gameState)
            |> Seq.toList
    
    
        PlayerMoved (displayInfo, playerMovementCapabilities)
    else
        let displayInfo = getFinalDisplayInfo gameState
        WonByCheckmate (displayInfo, gameState.turn)

and makeNextMoveInfo gameState playerAction =
    // the capability has the player & action to take & gameState baked in
    let executeFn() = executePlayerAction gameState playerAction
    { action = playerAction; execute = executeFn }

// player makes a move
and executePlayerAction gameState playerAction: PlayerActionOutcome =
    match playerAction with
    | MovePiece ply ->
        let newGameState = nextGameState ply gameState
        makePlayerMoveResultWithCapabilities newGameState
    | Resign ->
        let displayInfo = getFinalDisplayInfo gameState
        LostByResignation (displayInfo, (opponent gameState.turn))
    | OfferDraw ->
        let displayInfo = getDisplayInfo gameState
        let t = playerActions gameState
        let playerMovementCapabilities =
            t
            |> Seq.map (makeNextMoveInfo gameState)
            |> Seq.toList

        DrawOffer (displayInfo, gameState.turn, playerMovementCapabilities)

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
        enPassantPawn = None
    }

let newChessGame = makePlayerMoveResultWithCapabilities initialGameState
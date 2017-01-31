module Domain
open Utils

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
    static member fromInt i =
        match i with
        | 7 -> R1
        | 6 -> R2
        | 5 -> R3
        | 4 -> R4
        | 3 -> R5
        | 2 -> R6
        | 1 -> R7
        | 0 -> R8
        | _ -> failwith "Invalid rank index"

type File = A | B | C | D | E | F | G | H
with
    static member fromInt i =
        match i with
        | 0 -> A
        | 1 -> B
        | 2 -> C
        | 3 -> D
        | 4 -> E
        | 5 -> F
        | 6 -> G
        | 7 -> H
        | _ -> failwith "Invalid file index"

let nextRank = function
    | R1 -> Some R2
    | R2 -> Some R3
    | R3 -> Some R4
    | R4 -> Some R5
    | R5 -> Some R6
    | R6 -> Some R7
    | R7 -> Some R8
    | R8 -> None

let prevRank = function
    | R1 -> None
    | R2 -> Some R1
    | R3 -> Some R2
    | R4 -> Some R3
    | R5 -> Some R4
    | R6 -> Some R5
    | R7 -> Some R6
    | R8 -> Some R7

let nextFile = function
    | A -> Some B
    | B -> Some C
    | C -> Some D
    | D -> Some E
    | E -> Some F
    | F -> Some G
    | G -> Some H
    | H -> None

let prevFile = function
    | A -> None
    | B -> Some A
    | C -> Some B
    | D -> Some C
    | E -> Some D
    | F -> Some E
    | G -> Some F
    | H -> Some G

let sameRank = Some
let sameFile = Some

type Position = File * Rank

let simplifyTuple = function
    | (Some a, Some b) -> Some (a, b)
    | _ -> None

let move fileFn rankFn (f, r) = (fileFn f, rankFn r) |> simplifyTuple

let Up        = move sameFile nextRank 
let UpRight   = move nextFile nextRank 
let Right     = move nextFile sameRank 
let DownRight = move nextFile prevRank 
let Down      = move sameFile prevRank 
let DownLeft  = move prevFile prevRank 
let Left      = move prevFile sameRank 
let UpLeft    = move prevFile nextRank 


let extend nextFn =
    nextFn >> Option.map (fun x -> (x, x)) |> Seq.unfold

let bishopReaches pos =
    [ UpRight; DownRight; DownLeft; UpLeft ]
    |> List.map extend
    |> List.apply pos

let rookReaches pos =
    [ Up; Right; Down; Left ]
    |> List.map extend
    |> List.apply pos

let queenReaches pos =
    [ bishopReaches; rookReaches ]
    |> List.apply pos
    |> List.concat

let kingReaches pos =
    [ UpRight; DownRight; DownLeft; UpLeft; Up; Right; Down; Left ]
    |> List.apply pos
    |> List.choose id
    |> List.map Seq.singleton

let knightReaches pos =
    [
        Up >?> Left >?> Up;
        Up >?> Left >?> Left;
        Down >?> Left >?> Left;
        Down >?> Left >?> Left;
        Up >?> Right >?> Up;
        Up >?> Right >?> Right;
        Down >?> Right >?> Right;
        Down >?> Right >?> Right;
    ]
    |> List.apply pos
    |> List.choose id
    |> List.map Seq.singleton

let pawnMoveReaches rankForDoubleMove moveFn pos =
    match pos with
    | (_, rank) when rank = rankForDoubleMove ->
        extend moveFn pos
        |> Seq.take 2
        |> List.singleton
    | _ ->
        extend moveFn pos
        |> Seq.take 1
        |> List.singleton

let pawnCaptureReaches moveFn pos =
    [
        moveFn >?> Left;
        moveFn >?> Right;
    ]
    |> List.apply pos
    |> List.filterNones
    |> List.map Seq.singleton

let kingCastleToKingReach moveFn (pos: Position) =
    [ moveFn * 2; ]
    |> List.apply pos
    |> List.filterNones
    |> List.map Seq.singleton

let kingCastleToQueenReach moveFn (pos: Position) =
    [ moveFn * 3; ]
    |> List.apply pos
    |> List.filterNones
    |> List.map Seq.singleton

type Color = Black | White

let opposite = function
    | White -> Black
    | Black -> White

type Piece = Piece of Color * Shape

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

type Action =
    | Move
    | Capture
    | EnPassant
    | CastleKingSide
    | CastleQueenSide

type PlayerAction =
    | MovePiece of Piece * Position * Action * Position
    | Abandon

let pieceReachesGenerators = function
    | Piece (White, Pawn) ->
        [
            (pawnMoveReaches    R2 Up, [ Move               ]);
            (pawnCaptureReaches    Up, [ Capture; EnPassant ]);
        ]
    | Piece (Black, Pawn) ->
        [
            (pawnMoveReaches    R7 Down, [ Move      ]);
            (pawnCaptureReaches    Down, [ Capture; EnPassant ]);
        ]
    | Piece (_, Knight) -> [(knightReaches, [ Move; Capture ])]
    | Piece (_, Bishop) -> [(bishopReaches, [ Move; Capture ])]
    | Piece (_, Rook  ) -> [(rookReaches  , [ Move; Capture ])]
    | Piece (_, Queen ) -> [(queenReaches , [ Move; Capture ])]
    | Piece (White, King  ) ->
        [
            (kingReaches                 , [ Move; Capture   ]);
            (kingCastleToKingReach  Right, [ CastleKingSide  ]);
            (kingCastleToQueenReach Left , [ CastleQueenSide ]);
        ]
    | Piece (Black, King  ) ->
        [
            (kingReaches                 , [ Move; Capture   ]);
            (kingCastleToKingReach  Left , [ CastleKingSide  ]);
            (kingCastleToQueenReach Right, [ CastleQueenSide ]);
        ]

let sameColor (Piece (sourceColor, _)) square =
    match color square with
    | Some col -> col = sourceColor
    | None -> false
    
let differentColor p s = not (sameColor p s)

type CastleStatus = {
    kingHasMoved: bool
    kingsRookHasMoved: bool
    queensRookHasMoved: bool
}

type GameState = {
    whitePlayerCastleState: CastleStatus
    blackPlayerCastleState: CastleStatus
    enPassantPawn: Position option
    pieces: (Piece * Position) list
}

let canExecute game sourcePiece sourcePos targetPos action =
    let targetSquare = game.pieces @ targetPos
    match action with
    | Move ->
        targetSquare |> isEmpty
    | Capture ->
        targetSquare |> hasPiece &&
        targetSquare |> (differentColor sourcePiece)
    | EnPassant ->
        let passedPawnPos = (fst targetPos, snd sourcePos)

        // targetSquare |> isEmpty && // THIS SHOULD BE ALWAYS TRUE
        game.enPassantPawn
        |> Option.map (fun p -> p = passedPawnPos)
        |> Option.isSome

    | CastleKingSide ->
        false
    | CastleQueenSide ->
        false

let reachIsOpen = function
    | Some (_, _, Move, _) -> true
    | _ -> false

let evaluateSquare game actionsToAnalyze (sourcePiece, sourcePos) targetPos =
    actionsToAnalyze
    |> Seq.filter (canExecute game sourcePiece sourcePos targetPos)
    |> Seq.map (fun action -> (sourcePiece, sourcePos, action, targetPos))
    |> Seq.tryHead

let reachCapabilities game (piece, sourcePos) actionsToAnalyze reach =
    reach
    |> Seq.map (evaluateSquare game actionsToAnalyze (piece, sourcePos))
    |> Seq.takeWhilePlusOne reachIsOpen
    |> Seq.filterNones

let reachesCapabilities (game: GameState) (piece, sourcePos) (reachesGenerator, actionsToAnalyze) =
    sourcePos
    |> reachesGenerator
    |> Seq.collect (reachCapabilities game (piece, sourcePos) actionsToAnalyze)

let uncheckedPieceCapabilities game (piece, sourcePos) =
    piece
    |> pieceReachesGenerators
    |> Seq.collect (reachesCapabilities game (piece, sourcePos))

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacksBy game placedPiece =
    uncheckedPieceCapabilities game placedPiece
    |> Seq.filter (fun (_, _, action, _) -> action = Capture)
    |> Seq.map (fun (_, _, _, pos) -> pos)

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacks col game = 
    game.pieces
    |> Seq.filter (fun (Piece (pieceColor, _), _) -> pieceColor = col)
    |> Seq.collect (attacksBy game)

let isAttacked color game targetPosition =
    game
    |> attacks color
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
    |> Option.map (isAttacked color game)
    |> defaultArg <| false

let pieceCapabilities game (piece, sourcePos) =
    let checkFilter (Piece (color, _), _, _, targetPos) =
        { game with 
            pieces = 
                game.pieces |> rawMoveAndReplace (sourcePos, targetPos)
        }
        |> (not << isCheck color)

    uncheckedPieceCapabilities game (piece, sourcePos)
    |> Seq.filter checkFilter

type Player = Player of Color

type DisplayInfo = {
    board: Square[,]
    playerToMove: Player option
}

/// <summary>
/// Possible results of a player action result.
/// </summary>
type PlayerActionOutcome =
    | PlayerMoved of DisplayInfo * ExecutableAction list
    | WonByCheckMate of DisplayInfo * Player
    | WonByAbandon of DisplayInfo * Player
    | Draw of DisplayInfo * Player

/// <summary>
/// Information about one possible movement. It includes
/// the source and target position, and a function that, when
/// called, will return the result of the movement (a MoveResult).
/// </summary>
and ExecutableAction = {
    action: PlayerAction
    execute: unit -> PlayerActionOutcome
}

let opponent (Player playerColor) = Player (opposite playerColor)

let getFinalDisplayInfo game =
    {
        board = Array2D.init 8  8 (fun r f ->
            game.pieces @ (File.fromInt f, Rank.fromInt r)
        )
        playerToMove = None
    }

let getDisplayInfo game player =
    {
        board = Array2D.init 8  8 (fun r f ->
            game.pieces @ (File.fromInt f, Rank.fromInt r)
        )
        playerToMove = Some player
    }

let private isCheckMateTo player gameState =
    // TODO:
    // let (<&>) f g = (fun x -> f x && g x)
    // isCheck <&> capabilities 
    false

let private isGameTied player gameState = false

/// <summary>Return the move result case for a player.</summary>
let moveResultFor displayInfo nextMoves = 
    PlayerMoved (displayInfo, nextMoves)

let rec f x = x
// given a player & a gameState, it returns a move result for that player.
and makePlayerMoveResultWithCapabilities (Player playerColor) gameState =
    let displayInfo = getDisplayInfo gameState (Player playerColor)

    let getCapabilities (sourcePiece, sourcePos) =
        pieceCapabilities gameState (sourcePiece, sourcePos)
        |> Seq.map MovePiece

    let belongsToPlayer (Piece (pieceColor, _), _) = pieceColor = playerColor

    let playerMovementCapabilities =
        gameState.pieces
        |> Seq.filter belongsToPlayer
        |> Seq.collect getCapabilities
        |> Seq.map (makeNextMoveInfo (Player playerColor) gameState)
        |> Seq.toList

    moveResultFor displayInfo playerMovementCapabilities

and makeNextMoveInfo player gameState playerAction =
    // the capability has the player & action to take & gameState baked in
    let executeFn() = executePlayerAction player gameState playerAction
    { action = playerAction; execute = executeFn }

// player makes a move
and executePlayerAction player gameState playerAction: PlayerActionOutcome =
    match playerAction with
    | MovePiece (_, spos, action, tpos) ->
        let newGameState =
            { gameState with
                pieces = gameState.pieces |> rawMoveAndReplace (spos, tpos)
            }

        if newGameState |> isCheckMateTo player then
            let displayInfo = getFinalDisplayInfo newGameState
            WonByCheckMate (displayInfo, player)
        // TODO: Tie and abandonment
        else
            let otherPlayer = opponent player
            makePlayerMoveResultWithCapabilities otherPlayer newGameState
    | Abandon ->
        let displayInfo = getFinalDisplayInfo gameState
        WonByAbandon (displayInfo, (opponent player))

let newGame() =
    let initialPieces = [
        placedPiece White Rook   (A, R1)
        placedPiece White Knight (B, R1)
        placedPiece White Bishop (C, R1)
        placedPiece White Queen  (D, R1)
        placedPiece White King   (E, R1)
        placedPiece White Bishop (F, R1)
        placedPiece White Knight (G, R1)
        placedPiece White Rook   (H, R1)
        placedPiece White Pawn   (A, R2)
        placedPiece White Pawn   (B, R2)
        placedPiece White Pawn   (C, R2)
        placedPiece White Pawn   (D, R2)
        placedPiece White Pawn   (E, R2)
        placedPiece White Pawn   (F, R2)
        placedPiece White Pawn   (G, R2)
        placedPiece White Pawn   (H, R2)
        placedPiece Black Pawn   (A, R7)
        placedPiece Black Pawn   (B, R7)
        placedPiece Black Pawn   (C, R7)
        placedPiece Black Pawn   (D, R7)
        placedPiece Black Pawn   (E, R7)
        placedPiece Black Pawn   (F, R7)
        placedPiece Black Pawn   (G, R7)
        placedPiece Black Pawn   (H, R7)
        placedPiece Black Rook   (A, R8)
        placedPiece Black Knight (B, R8)
        placedPiece Black Bishop (C, R8)
        placedPiece Black Queen  (D, R8)
        placedPiece Black King   (E, R8)
        placedPiece Black Bishop (F, R8)
        placedPiece Black Knight (G, R8)
        placedPiece Black Rook   (H, R8)
    ]

    makePlayerMoveResultWithCapabilities (Player White) {
        pieces = initialPieces
        whitePlayerCastleState = {
                                    kingHasMoved = false
                                    kingsRookHasMoved = false
                                    queensRookHasMoved = false
                                 }
        blackPlayerCastleState = {
                                    kingHasMoved = false
                                    kingsRookHasMoved = false
                                    queensRookHasMoved = false
                                 }
        enPassantPawn = None
    }
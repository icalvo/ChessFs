module Domain
open Utils

// CORE TYPES
type Chessman =
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

let pawnEnPassantReach (rankForEnPassant: Rank) moveFn (pos: Position) =
    match pos with
    | (_, rank) when rank = rankForEnPassant ->
        [
            moveFn >?> Left;
            moveFn >?> Right;
        ]
        |> List.apply pos
        |> List.filterNones
        |> List.map Seq.singleton
    | _ -> []


type Color = Black | White

type Piece = Piece of Color * Chessman

type CellState = 
    | Cell of Piece * Position
    | Empty of Position

let cell color chessman pos = (Piece (color, chessman), pos)

let isEmpty = function
    | Empty _ -> true
    | _ -> false

let color = function
    | Cell (Piece (col, _), _) -> Some col
    | _ -> None
    
let piece = function
    | Cell (piece, _) -> Some piece
    | _ -> None

let position = function
    | Cell (_, pos) -> pos
    | Empty pos -> pos

let at pos game =
    game
    |> List.map Cell
    |> List.tryFind (fun cell -> position cell = pos)
    |> defaultArg <| Empty pos

let (@) game pos = at pos game

type Action =
    | Move
    | Capture
    | EnPassant

type PlayerAction =
    | MovePiece of Piece * Position * Action * Position
    | Abandon

let pieceReachesGenerators = function
    | Piece (White, Pawn) ->
        [
            (pawnMoveReaches    R2 Up, [ Move      ]);
            (pawnCaptureReaches    Up, [ Capture   ]);
            (pawnEnPassantReach R5 Up, [ EnPassant ])
        ]
    | Piece (Black, Pawn) ->
        [
            (pawnMoveReaches R7 Down,    [ Move      ]);
            (pawnCaptureReaches Down,    [ Capture   ]);
            (pawnEnPassantReach R4 Down, [ EnPassant ])
        ]
    | Piece (_, Knight) -> [(knightReaches, [ Move; Capture ])]
    | Piece (_, Bishop) -> [(bishopReaches, [ Move; Capture ])]
    | Piece (_, Rook  ) -> [(rookReaches  , [ Move; Capture ])]
    | Piece (_, Queen ) -> [(queenReaches , [ Move; Capture ])]
    | Piece (_, King  ) -> [(kingReaches  , [ Move; Capture ])]

let sameColor (Piece (sourceColor, chessman)) cell2 =
    match color cell2 with
    | Some col -> col = sourceColor
    | None -> false

let evaluateSquareAction game targetPos (piece, sourcePos) action =
    let targetCell = game @ targetPos
    match action with
    | Move ->
        targetCell |> isEmpty
    | Capture ->
        targetCell |> (not << sameColor piece) &&
        targetCell |> (not << isEmpty)
    | EnPassant ->
        isEmpty targetCell

let reachIsOpen = function
    | Some (_, _, Move, _) -> true
    | _ -> false

let evaluateSquare game actionsToAnalyze (piece, sourcePos) targetPos =
    actionsToAnalyze
    |> Seq.filter (evaluateSquareAction game targetPos (piece, sourcePos))
    |> Seq.map (fun action -> (piece, sourcePos, action, targetPos))
    |> Seq.tryHead

let reachCapabilities game (piece, sourcePos) actionsToAnalyze reach =
    reach
    |> Seq.map (evaluateSquare game actionsToAnalyze (piece, sourcePos))
    |> Seq.takeWhilePlusOne reachIsOpen
    |> Seq.filterNones

type GameState = (Piece * Position) list

let reachesCapabilities (game: GameState) (piece, sourcePos) (reachesGenerator, actionsToAnalyze) =
    sourcePos
    |> reachesGenerator
    |> Seq.collect (reachCapabilities game (piece, sourcePos) actionsToAnalyze)

let uncheckedPieceCapabilities game (piece, sourcePos) =
    piece
    |> pieceReachesGenerators
    |> Seq.collect (reachesCapabilities game (piece, sourcePos))

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacksBy game cellState =
    uncheckedPieceCapabilities game cellState
    |> Seq.filter (fun (_, _, action, _) -> action = Capture)
    |> Seq.map (fun (_, _, _, pos) -> pos)

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacks col game = 
    game
    |> Seq.filter (fun (Piece (pieceColor, _), _) -> pieceColor = col)
    |> Seq.collect (attacksBy game)

let isAttacked color game targetPosition =
    game
    |> attacks color
    |> Seq.contains targetPosition

/// <summary>Basic move operation. It simply empties the source cell and sets the target cell with the
/// same the source had before.</summary>
let moveAndReplace (sourcePos, targetPos) game =
    match game @ sourcePos with
    | Empty _ -> game |> List.filter (fun (_, position) -> position <> targetPos)
    | Cell (piece, _) -> (piece, targetPos) :: (game |> List.filter (fun (_, position) -> position <> sourcePos && position <> targetPos))


let isCheck color game =
    game
    |> Seq.tryFind (fun (piece, pos) -> piece = Piece (color, King))
    |> Option.map (fun (piece, pos) -> pos)
    |> Option.map (isAttacked color game)
    |> defaultArg <| false

let pieceCapabilities game (piece, sourcePos) =
    let checkFilter (Piece (color, _), _, _, targetPos) =
        game
        |> moveAndReplace (sourcePos, targetPos)
        |> (not << isCheck color)

    uncheckedPieceCapabilities game (piece, sourcePos)
    |> Seq.filter checkFilter

let capabilities game sourcePos =
    match game @ sourcePos with
    | Empty _ -> Seq.empty
    | Cell (piece, _) ->
        (piece, sourcePos)
        |> pieceCapabilities game

type Player = Color

type DisplayInfo = CellState[,]

/// <summary>
/// Possible results of a player action result.
/// </summary>
type PlayerActionResult =
    | PlayerWhiteToMove of DisplayInfo * NextMoveInfo list
    | PlayerBlackToMove of DisplayInfo * NextMoveInfo list
    | WonByCheckMate of DisplayInfo * Player
    | WonByAbandon of DisplayInfo * Player
    | Draw of DisplayInfo * Player

/// <summary>
/// Information about one possible movement. It includes
/// the source and target position, and a function that, when
/// called, will return the result of the movement (a MoveResult).
/// </summary>
and NextMoveInfo = {
    movement: PlayerAction
    capability: unit -> PlayerActionResult
}

let opponent = function
    | White -> Black
    | Black -> White

let getDisplayInfo game =
    Array2D.init 8  8 (fun r f ->
        game @ (File.fromInt f, Rank.fromInt r)
    )

let private isCheckMateTo player gameState =
    // TODO:
    // let (<&>) f g = (fun x -> f x && g x)
    // isCheck <&> capabilities 
    false

let private isGameTied player gameState = false

let private remainingMoves gameState: PlayerAction list = []

/// <summary>Return the move result case for a player.</summary>
let moveResultFor player displayInfo nextMoves = 
    match player with
    | White -> PlayerWhiteToMove (displayInfo, nextMoves)
    | Black -> PlayerBlackToMove (displayInfo, nextMoves)

let rec f x = x
// given a player & a gameState, it returns a move result for that player.
and makePlayerMoveResultWithCapabilities player gameState =
    let displayInfo = getDisplayInfo gameState

    let getCapabilities (sourcePiece, sourcePos) =
        pieceCapabilities gameState (sourcePiece, sourcePos)
        |> Seq.map MovePiece

    let belongsToPlayer (Piece (color, _), _) = color = player

    let playerMovementCapabilities =
        gameState
        |> Seq.filter belongsToPlayer
        |> Seq.collect getCapabilities
        |> Seq.map (makeNextMoveInfo player gameState)
        |> Seq.toList

    moveResultFor player displayInfo playerMovementCapabilities

and makeNextMoveInfo player gameState playerAction =
    // the capability has the player & cellPos & gameState baked in
    let capability() = executePlayerAction player gameState playerAction
    { movement = playerAction; capability = capability }

// player makes a move
and executePlayerAction player gameState playerAction: PlayerActionResult =
    match playerAction with
    | MovePiece (_, spos, _, tpos) ->
        let newGameState = gameState |> moveAndReplace (spos, tpos)
        let displayInfo = getDisplayInfo newGameState 

        if newGameState |> isCheckMateTo player then
            // return the move result
            WonByCheckMate (displayInfo, player)
        // TODO: Tie and abandonment
        else
            let otherPlayer = opponent player
            makePlayerMoveResultWithCapabilities otherPlayer newGameState
    | Abandon ->
        let displayInfo = getDisplayInfo gameState
        WonByAbandon (displayInfo, (opponent player))

let newGame() =
    let initialCells = [
        cell White Rook   (A, R1)
        cell White Knight (B, R1)
        cell White Bishop (C, R1)
        cell White Queen  (D, R1)
        cell White King   (E, R1)
        cell White Bishop (F, R1)
        cell White Knight (G, R1)
        cell White Rook   (H, R1)
        cell White Pawn   (A, R2)
        cell White Pawn   (B, R2)
        cell White Pawn   (C, R2)
        cell White Pawn   (D, R2)
        cell White Pawn   (E, R2)
        cell White Pawn   (F, R2)
        cell White Pawn   (G, R2)
        cell White Pawn   (H, R2)
        cell Black Pawn   (A, R7)
        cell Black Pawn   (B, R7)
        cell Black Pawn   (C, R7)
        cell Black Pawn   (D, R7)
        cell Black Pawn   (E, R7)
        cell Black Pawn   (F, R7)
        cell Black Pawn   (G, R7)
        cell Black Pawn   (H, R7)
        cell Black Rook   (A, R8)
        cell Black Knight (B, R8)
        cell Black Bishop (C, R8)
        cell Black Queen  (D, R8)
        cell Black King   (E, R8)
        cell Black Bishop (F, R8)
        cell Black Knight (G, R8)
        cell Black Rook   (H, R8)
    ]

    makePlayerMoveResultWithCapabilities White initialCells
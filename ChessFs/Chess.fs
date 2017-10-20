module Chess
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

let bishopReaches pos =
    [ UpRight; DownRight; DownLeft; UpLeft ]
    |> List.map Seq.unfold2
    |> List.apply pos

let rookReaches pos =
    [ Up; Right; Down; Left ]
    |> List.map Seq.unfold2
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
        Seq.unfold2 moveFn pos
        |> Seq.take 2
        |> List.singleton
    | _ ->
        Seq.unfold2 moveFn pos
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

let WhitePawn = Piece (White, Pawn)
let BlackPawn = Piece (Black, Pawn)

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

type Ply =
    | Move of Piece * Position * Position
    | Capture of Piece * Position * Position
    | EnPassant of Piece * Position * Position
    | CastleKingSide of Color
    | CastleQueenSide of Color
    | Promote of Piece * Position * Position * Shape
    | CaptureAndPromote of Piece * Position * Position * Shape

let plyPositions = function
    | Move (_, s, t) -> (s, t)
    | Capture (_, s, t) -> (s, t)
    | EnPassant (_, s, t) -> (s, t)
    | CastleKingSide White -> (E1, G1)
    | CastleQueenSide White -> (E1, C1)
    | CastleKingSide Black -> (E8, G8)
    | CastleQueenSide Black -> (E8, C8)
    | Promote (_, s, t, _) -> (s, t)
    | CaptureAndPromote (_, s, t, _) -> (s, t)


type PlayerAction =
    | MovePiece of Ply
    | Resign
    | OfferDraw

type PieceAction =
    | Move
    | Capture
    | EnPassant
    | CastleKingSide
    | CastleQueenSide
    | Promote
    | CaptureAndPromote

let whitePawnMoveReaches =    pawnMoveReaches    R2 Up
let whitePawnCaptureReaches = pawnCaptureReaches    Up
let blackPawnMoveReaches =    pawnMoveReaches    R7 Down
let blackPawnCaptureReaches = pawnCaptureReaches    Down

let pieceReaches2 piece pos =
    let fn2 ls =
        ls |> List.collect (fun (reachesfn, act) -> pos |> reachesfn |> List.map (fun (reach) -> (reach, act)))

    match piece with
    | Piece (White, Pawn) ->
        match pos with
        | (_, R7) ->
            fn2 [
                (whitePawnMoveReaches, Promote);
                (whitePawnCaptureReaches, CaptureAndPromote)]
        | (_, R5) ->
            fn2 [
                (whitePawnMoveReaches, Move);
                (whitePawnCaptureReaches, Capture);
                (whitePawnCaptureReaches, EnPassant)]
        | _ ->
            fn2 [
                (whitePawnMoveReaches   , Move   );
                (whitePawnCaptureReaches, Capture);
            ]
    | Piece (Black, Pawn) ->
        match pos with
        | (_, R2) ->
            [
                //(blackPawnMoveReaches    pos, [ Promote           ]);
                //(blackPawnCaptureReaches pos, [ CaptureAndPromote ]);
            ]
        | (_, R4) ->
            [
                //(blackPawnMoveReaches    pos, [ Move      ]);
                //(blackPawnCaptureReaches pos, [ Capture; EnPassant ]);
            ]
        | _ ->
            [
                //(blackPawnMoveReaches    pos, [ Move    ]);
                //(blackPawnCaptureReaches pos, [ Capture ]);
            ]
    | Piece (_, Knight) -> fn2 [(knightReaches, Move);(knightReaches, Capture)]
    | Piece (_, Bishop) -> fn2 [(bishopReaches, Move);(bishopReaches, Capture)]
    | Piece (_, Rook  ) -> fn2 [(rookReaches, Move);(bishopReaches, Capture)]
    | Piece (_, Queen ) -> fn2 [(queenReaches, Move);(queenReaches, Capture)]
    | Piece (White, King  ) ->
        fn2 [
            (kingReaches, Move);
            (kingReaches, Capture);
            (kingCastleToKingReach Right, CastleKingSide);
            (kingCastleToQueenReach Left, CastleQueenSide)
        ]
    | Piece (Black, King  ) ->
        [
            //(kingReaches pos                 , [ Move; Capture   ]);
            //(kingCastleToKingReach  Left pos , [ CastleKingSide  ]);
            //(kingCastleToQueenReach Right pos, [ CastleQueenSide ]);
        ]

let pieceReaches piece pos =
    match piece with
    | Piece (White, Pawn) ->
        match pos with
        | (_, R7) ->
            [
                (whitePawnMoveReaches    pos, [ Promote           ]);
                (whitePawnCaptureReaches pos, [ CaptureAndPromote ]);
            ]
        | (_, R5) ->
            [
                (whitePawnMoveReaches    pos, [ Move               ]);
                (whitePawnCaptureReaches pos, [ Capture; EnPassant ]);
            ]
        | _ ->
            [
                (whitePawnMoveReaches    pos, [ Move    ]);
                (whitePawnCaptureReaches pos, [ Capture ]);
            ]
    | Piece (Black, Pawn) ->
        match pos with
        | (_, R2) ->
            [
                (blackPawnMoveReaches    pos, [ Promote           ]);
                (blackPawnCaptureReaches pos, [ CaptureAndPromote ]);
            ]
        | (_, R4) ->
            [
                (blackPawnMoveReaches    pos, [ Move      ]);
                (blackPawnCaptureReaches pos, [ Capture; EnPassant ]);
            ]
        | _ ->
            [
                (blackPawnMoveReaches    pos, [ Move    ]);
                (blackPawnCaptureReaches pos, [ Capture ]);
            ]
    | Piece (_, Knight) -> [(knightReaches pos, [ Move; Capture ])]
    | Piece (_, Bishop) -> [(bishopReaches pos, [ Move; Capture ])]
    | Piece (_, Rook  ) -> [(rookReaches pos  , [ Move; Capture ])]
    | Piece (_, Queen ) -> [(queenReaches pos , [ Move; Capture ])]
    | Piece (White, King  ) ->
        [
            (kingReaches pos                 , [ Move; Capture   ]);
            (kingCastleToKingReach  Right pos, [ CastleKingSide  ]);
            (kingCastleToQueenReach Left pos , [ CastleQueenSide ]);
        ]
    | Piece (Black, King  ) ->
        [
            (kingReaches pos                 , [ Move; Capture   ]);
            (kingCastleToKingReach  Left pos , [ CastleKingSide  ]);
            (kingCastleToQueenReach Right pos, [ CastleQueenSide ]);
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

let canExecute game sourcePiece sourcePos targetPos action =
    let targetSquare = game.pieces @ targetPos
    match action with
    | Move | Promote ->
        targetSquare |> isEmpty
    | Capture | CaptureAndPromote ->
        targetSquare |> hasPiece &&
        targetSquare |> (differentColor sourcePiece)
    | EnPassant ->
        let passedPawnPos = (fst targetPos, snd sourcePos)

        // targetSquare |> isEmpty && // THIS SHOULD BE ALWAYS TRUE
        game.enPassantPawn
        |> Option.map (fun p -> p = passedPawnPos)
        |> Option.isSome

    | _ ->
        false

let reachIsOpen = function
    | Some (_, _, Move, _) -> true
    | _ -> false

let toPlies (Piece (color, sh), s, a, t) =
    let p = Piece (color, sh)
    match a with
    | Move -> [ Ply.Move (p, s, t) ]
    | Capture -> [ Ply.Capture (p, s, t) ]
    | EnPassant -> [ Ply.EnPassant (p, s, t) ]
    | CastleKingSide -> [ Ply.CastleKingSide color ]
    | CastleQueenSide -> [ Ply.CastleQueenSide color ]
    | Promote -> [ Queen; Rook; Bishop; Knight ] |> List.map (fun shape -> Ply.Promote (p, s, t, shape))
    | CaptureAndPromote -> [ Queen; Rook; Bishop; Knight ] |> List.map (fun shape -> Ply.CaptureAndPromote (p, s, t, shape))

let evaluateSquare game actionToAnalyze (sourcePiece, sourcePos) targetPos =
    let can = canExecute game sourcePiece sourcePos targetPos actionToAnalyze
    if can then Some (sourcePiece, sourcePos, actionToAnalyze, targetPos) else None

let reachCapabilities game (piece, sourcePos) actionToAnalyze reach =
    reach
    |> Seq.map (evaluateSquare game actionToAnalyze (piece, sourcePos))
    |> Seq.takeWhilePlusOne reachIsOpen
    |> Seq.filterNones

let uncheckedPieceCapabilities game (piece, sourcePos) =
    pieceReaches2 piece sourcePos
    |> Seq.collect (fun (r, a) -> reachCapabilities game (piece, sourcePos) a r)

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacksBy game placedPiece =
    uncheckedPieceCapabilities game placedPiece
    |> Seq.filter (fun (_, _, action, _) -> action = Capture || action = CaptureAndPromote)
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

type DisplayInfo = {
    board: Square[,]
    playerToMove: Color option
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

let opponent = opposite

let getFinalDisplayInfo game =
    {
        board = Array2D.init 8  8 (fun r f ->
            game.pieces @ (File.fromInt f, Rank.fromInt r)
        )
        playerToMove = None
    }

let getDisplayInfo game =
    {
        board = Array2D.init 8  8 (fun r f ->
            game.pieces @ (File.fromInt f, Rank.fromInt r)
        )
        playerToMove = Some game.turn
    }

let private isCheckmateTo player gameState =
    // TODO:
    // let (<&>) f g = (fun x -> f x && g x)
    // isCheck <&> capabilities 
    false

let private isGameTied player gameState = false

/// <summary>Return the move result case for a player.</summary>
let moveResultFor displayInfo nextMoves = 
    PlayerMoved (displayInfo, nextMoves)

// given a player & a gameState, it returns the possible player actions.
let playerActions gameState =
    printfn "Calculating playerActions..."
    let getCapabilities (sourcePiece, sourcePos) =
        pieceCapabilities gameState (sourcePiece, sourcePos)
        |> Seq.collect (toPlies)
        |> Seq.map MovePiece

    let belongsToPlayer (Piece (pieceColor, _), _) = pieceColor = gameState.turn
    
    gameState.pieces
    |> Seq.filter belongsToPlayer
    |> Seq.collect getCapabilities
    |> Seq.append [Resign; OfferDraw]


// given a player & a gameState, it returns a move result for that player.
let rec makePlayerMoveResultWithCapabilities gameState =
    let displayInfo = getDisplayInfo gameState
    let t = playerActions gameState
    let playerMovementCapabilities =
        t
        |> Seq.map (makeNextMoveInfo gameState)
        |> Seq.toList

    moveResultFor displayInfo playerMovementCapabilities

and makeNextMoveInfo gameState playerAction =
    // the capability has the player & action to take & gameState baked in
    let executeFn() = executePlayerAction gameState playerAction
    { action = playerAction; execute = executeFn }

// player makes a move
and executePlayerAction gameState playerAction: PlayerActionOutcome =
    match playerAction with
    | MovePiece ply ->
        let otherPlayer = opponent gameState.turn
        let newGameState =
            { gameState with
                turn = otherPlayer
                pieces = gameState.pieces |> rawMoveAndReplace (plyPositions ply)
            }

        if newGameState |> isCheckmateTo gameState.turn then
            let displayInfo = getFinalDisplayInfo newGameState
            WonByCheckmate (displayInfo, gameState.turn)
        // TODO: Tie and abandonment
        else
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


let newChessGame =
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

    makePlayerMoveResultWithCapabilities {
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
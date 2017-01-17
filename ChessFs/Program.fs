type Chessman =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King

type File = A | B | C | D | E | F | G | H

type Rank =
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8

let nextFile = function
    | F1 -> Some F2
    | F2 -> Some F3
    | F3 -> Some F4
    | F4 -> Some F5
    | F5 -> Some F6
    | F6 -> Some F7
    | F7 -> Some F8
    | F8 -> None

let prevFile = function
    | F1 -> None
    | F2 -> Some F1
    | F3 -> Some F2
    | F4 -> Some F3
    | F5 -> Some F4
    | F6 -> Some F5
    | F7 -> Some F6
    | F8 -> Some F7

let fileToInt = function
    | F1 -> 1
    | F2 -> 2
    | F3 -> 3
    | F4 -> 4
    | F5 -> 5
    | F6 -> 6
    | F7 -> 7
    | F8 -> 8

let nextRank = function
    | A -> Some B
    | B -> Some C
    | C -> Some D
    | D -> Some E
    | E -> Some F
    | F -> Some G
    | G -> Some H
    | H -> None

let prevRank = function
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

let move rankFn fileFn (a, b) = (fileFn a, rankFn b) |> simplifyTuple

type Movement = Position -> Position option

let Up       : Movement = move sameFile nextRank 
let UpRight  : Movement = move nextFile nextRank 
let Right    : Movement = move nextFile sameRank 
let DownRight: Movement = move nextFile prevRank 
let Down     : Movement = move sameFile prevRank 
let DownLeft : Movement = move prevFile prevRank 
let Left     : Movement = move prevFile sameRank 
let UpLeft   : Movement = move prevFile nextRank 

let extend (moveFn: Movement) =
    moveFn >> Option.map (fun x -> (x, x)) |> Seq.unfold

let Listapply arg fnlist = fnlist |> List.map (fun fn -> fn arg)

let bishopReaches pos =
    [ UpRight; DownRight; DownLeft; UpLeft ]
    |> List.map extend
    |> Listapply pos

let rookReaches pos =
    [ Up; Right; Down; Left ]
    |> List.map extend
    |> Listapply pos

let queenReaches pos =
    [ bishopReaches; rookReaches ]
    |> Listapply pos
    |> List.concat

let kingReaches pos =
    [ UpRight; DownRight; DownLeft; UpLeft; Up; Right; Down; Left ]
    |> Listapply pos
    |> List.choose id
    |> List.map Seq.singleton

let (>?>) f1 f2 = f1 >> Option.bind f2

let (|?>) x f2 = Some x |> Option.bind f2

let knightReaches pos =
    [
        Up >?> Left >?> Up;
        Up >?> Left >?> Left;
        Down >?> Left >?> Left;
        Down >?> Left >?> Left;
    ]
    |> Listapply pos
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
    |> Listapply pos
    |> List.choose id
    |> List.map Seq.singleton

let pawnEnPassantReach (rankForEnPassant: Rank) moveFn (pos: Position) =
    match pos with
    | (_, rank) when rank = rankForEnPassant ->
        [
            moveFn >?> Left;
            moveFn >?> Right;
        ]
        |> Listapply pos
        |> List.choose id
        |> List.map Seq.singleton
    | _ -> []

type Colour = Black | White

type Piece = Colour * Chessman

type CellState = Piece * Position

type GameState = CellState list

let (@) (game: GameState) (pos: Position): CellState option =
    List.tryFind (fun (_, p) -> p = pos) game

type Action =
    | Move
    | Capture
    | EnPassant

let pieceReaches = function
    | (White, Pawn) ->
        [
            (pawnMoveReaches    F2 Up, [ Move      ]);
            (pawnCaptureReaches    Up, [ Capture   ]);
            (pawnEnPassantReach F5 Up, [ EnPassant ])
        ]
    | (Black, Pawn) ->
        [
            (pawnMoveReaches F7 Down,    [ Move      ]);
            (pawnCaptureReaches Down,    [ Capture   ]);
            (pawnEnPassantReach F4 Down, [ EnPassant ])
        ]
    | (_, Knight) -> [(knightReaches, [ Move; Capture ])]
    | (_, Bishop) -> [(bishopReaches, [ Move; Capture ])]
    | (_, Rook  ) -> [(rookReaches  , [ Move; Capture ])]
    | (_, Queen ) -> [(queenReaches , [ Move; Capture ])]
    | (_, King  ) -> [(kingReaches  , [ Move; Capture ])]

let color (cell: CellState) = cell |> fst |> fst

let piece (cell: CellState) = cell |> fst

let position (cell: CellState) = cell |> snd

let sameColor cell1 cell2 =
    color cell1 = color cell2

let takeWhilePlusOne predicate (s:seq<_>) = 
    /// Iterates over the enumerator, yielding elements and
    /// stops after an element for which the predicate does not hold
    let rec loop (en:System.Collections.Generic.IEnumerator<_>) = seq {
        if en.MoveNext() then
            // Always yield the current, stop if predicate does not hold
            yield en.Current
            if predicate en.Current then
                yield! loop en }

    // Get enumerator of the sequence and yield all results
    // (making sure that the enumerator gets disposed)
    seq { use en = s.GetEnumerator()
        yield! loop en }

module Option =
    let invert opt def =
        match opt with
        | Some _ -> None
        | None -> Some def

let evaluateSquareAction game target cellToMove action =
    let targetCell = game @ target
    let filteredTarget =
        match action with
        | Move -> Option.invert targetCell target
        | Capture ->
            targetCell
            |> Option.filter (not << sameColor cellToMove) 
            |> Option.map (fun _ -> target)
        | EnPassant -> Option.invert targetCell target
    filteredTarget |> Option.map (fun tgt -> (action, tgt))

let reachBreak = function
    | Some (Move, _) -> true
    | _ -> false

let evaluateSquare game actionsToAnalyze cellToMove target =
    actionsToAnalyze
    |> List.map (evaluateSquareAction game target cellToMove)
    |> Seq.choose id
    |> Seq.tryHead

let reachesCapabilities game cellToMove actionsToAnalyze reach =
    reach
    |> Seq.map (evaluateSquare game actionsToAnalyze cellToMove)
    |> takeWhilePlusOne reachBreak
    |> Seq.choose id
    |> Seq.toList

let reachesListCapabilities (game: GameState) (cellToMove: CellState) (reachGenerator, actionsToAnalyze) =
    cellToMove
    |> position
    |> reachGenerator
    |> Seq.collect (reachesCapabilities game cellToMove actionsToAnalyze)

let pieceCapabilities game cellToMove =
    cellToMove
    |> piece
    |> pieceReaches
    |> Seq.collect (reachesListCapabilities game cellToMove)
    |> Seq.map (fun (action, pos) -> (cellToMove, action, pos))
    |> Seq.toList

let isCheck game = false

let execute game ((piece, sourcePos): CellState) (targetPos: Position) =
    (piece, targetPos) :: (game |> List.filter (fun x -> position x = sourcePos) |> List.filter (fun x -> position x = targetPos))

let defaultArg2 def opt = defaultArg opt def

let capabilities game pos =
    game @ pos
    |> Option.map (pieceCapabilities game)
    |> defaultArg2 []
    |> List.filter (fun (cs, act, p) -> isCheck (execute game cs p))


let positionToString (r, f) =
    sprintf "%A%i" r (fileToInt f)

let printPositions (l: Position list) =
    l |> List.map positionToString |> List.iter (printf "%A")

// TESTING

let game1: GameState = [
    ((White, Bishop), (A, F1))
]

let game2: GameState = [
    ((White, Bishop), (A, F1))
    ((White, Knight), (C, F3))
]

capabilities game2 (A, F1)
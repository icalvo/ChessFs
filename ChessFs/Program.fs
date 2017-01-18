// UTILS

module Option =
    let invert opt def =
        match opt with
        | Some _ -> None
        | None -> Some def
    let pipe action = Option.map (fun x -> action x; x)

module Seq =
    let pipe action = Seq.map (fun x -> action x; x)
    let debug name = pipe (printfn "%s yields %A" name)
    let filterNones (s: seq<'a option>): seq<'a> = Seq.choose id s
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

module List =
    let pipe action = List.map (fun x -> action x; x)
    let debug name = pipe (printfn "%s yields %A" name)
    let apply arg fnlist = fnlist |> List.map (fun fn -> fn arg)
    let filterNones (s: 'a option list): 'a list = List.choose id s

// CORE TYPES
[<StructuredFormatDisplay("{toString}")>]
type Chessman =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King
with
    member this.toString = 
        match this with
        | Pawn   -> "P"
        | Knight -> "N"
        | Bishop -> "B"
        | Rook   -> "R"
        | Queen  -> "Q"
        | King   -> "K"

[<StructuredFormatDisplay("{toString}")>]
type Rank =
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
with
    member this.toString =
        match this with
        | R1 -> "1"
        | R2 -> "2"
        | R3 -> "3"
        | R4 -> "4"
        | R5 -> "5"
        | R6 -> "6"
        | R7 -> "7"
        | R8 -> "8"
    static member fromInt i =
        match i with
        | 0 -> R1
        | 1 -> R2
        | 2 -> R3
        | 3 -> R4
        | 4 -> R5
        | 5 -> R6
        | 6 -> R7
        | 7 -> R8
        | _ -> failwith "Invalid rank index"

type File = A | B | C | D | E | F | G | H
with
    static member fromInt i =
        match i with
        | 7 -> A
        | 6 -> B
        | 5 -> C
        | 4 -> D
        | 3 -> E
        | 2 -> F
        | 1 -> G
        | 0 -> H
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

let (>?>) f1 f2 = f1 >> Option.bind f2

let (|?>) x f2 = Some x |> Option.bind f2

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


[<StructuredFormatDisplay("{toString}")>]
type Colour = Black | White with
    member this.toString =
        match this with
        | Black -> "b"
        | White -> "w"

type Piece = Colour * Chessman

type CellState = Piece * Position

type GameState = CellState list

let at (pos: Position) (game: GameState): CellState option =
    List.tryFind (fun (_, p) -> p = pos) game

let (@) (game: GameState) (pos: Position): CellState option =
    List.tryFind (fun (_, p) -> p = pos) game

type Action =
    | Move
    | Capture
    | EnPassant

let pieceReachesGenerators = function
    | (White, Pawn) ->
        [
            (pawnMoveReaches    R2 Up, [ Move      ]);
            (pawnCaptureReaches    Up, [ Capture   ]);
            (pawnEnPassantReach R5 Up, [ EnPassant ])
        ]
    | (Black, Pawn) ->
        [
            (pawnMoveReaches R7 Down,    [ Move      ]);
            (pawnCaptureReaches Down,    [ Capture   ]);
            (pawnEnPassantReach R4 Down, [ EnPassant ])
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

let ff cellToMove target targetCell =
    function
    | Move -> Option.invert targetCell target
    | Capture ->
        targetCell
        |> Option.filter (not << sameColor cellToMove) 
        |> Option.map (fun _ -> target)
    | EnPassant -> Option.invert targetCell target

let evaluateSquareAction game target cellToMove action =
    let targetCell = game @ target
    ff cellToMove target targetCell action
    |> Option.map (fun tgt -> (action, tgt))

let reachIsBlocked = function
    | Some (Move, _) -> true
    | _ -> false

let evaluateSquare game actionsToAnalyze cellToMove target =
    actionsToAnalyze
    |> List.map (evaluateSquareAction game target cellToMove)
    |> Seq.choose id
    |> Seq.tryHead

let reachCapabilities game cellToMove actionsToAnalyze reach =
    reach
    |> Seq.map (evaluateSquare game actionsToAnalyze cellToMove)
    |> Seq.takeWhilePlusOne reachIsBlocked
    |> Seq.choose id

let reachesCapabilities (game: GameState) (cellToMove: CellState) (reachesGenerator, actionsToAnalyze) =
    cellToMove
    |> position
    |> reachesGenerator
    |> Seq.collect (reachCapabilities game cellToMove actionsToAnalyze)

let pieceCapabilities game cellToMove =
    cellToMove
    |> piece
    |> pieceReachesGenerators
    |> Seq.collect (reachesCapabilities game cellToMove)

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacksBy game cellState =
    pieceCapabilities game cellState
    |> Seq.filter (fun (action, pos) -> action = Capture)
    |> Seq.map snd

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacks col game = 
    game
    |> List.toSeq
    |> Seq.filter (fun cell -> color cell = col)
    |> Seq.collect (attacksBy game >> Seq.toList) 

let isAttacked color game targetPosition =
    game
    |> attacks color
    |> Seq.contains targetPosition
    
let defaultArg2 def opt = defaultArg opt def

let isCheck color game =
    game
    |> List.tryFind (fun x -> piece x = (color, King))
    |> Option.map position
    |> Option.map (isAttacked color game)
    |> defaultArg2 false

let moveAndReplace ((piece, sourcePos): CellState) (targetPos: Position) game =
    (piece, targetPos) :: (game |> List.filter (fun x -> position x <> sourcePos && position x <> targetPos))

let capabilities game pos =
    game @ pos
    |> Option.map (fun cell ->
        cell
        |> pieceCapabilities game
        |> Seq.debug "capabilityBeforeCheckTest"
        |> Seq.filter (fun (act, p) -> (moveAndReplace cell p game) |> (not << isCheck (color cell))))
    |> Option.toList

let positionToString ((f, r):Position) =
    sprintf "%A%A" f r

let printPositions =
    List.map positionToString >> List.iter (printf "%A")

// TESTING
let game1: GameState = [
    ((White, Bishop), (A, R1))
]

let game2: GameState = [
    ((White, Bishop), (A, R1))
    ((White, Knight), (C, R3))
]

let init: GameState = [
    ((White, Rook  ), (A, R1))
    ((White, Knight), (B, R1))
    ((White, Bishop), (C, R1))
    ((White, Queen ), (D, R1))
    ((White, King  ), (E, R1))
    ((White, Bishop), (F, R1))
    ((White, Knight), (G, R1))
    ((White, Rook  ), (H, R1))
    ((White, Pawn  ), (A, R2))
    ((White, Pawn  ), (B, R2))
    ((White, Pawn  ), (C, R2))
    ((White, Pawn  ), (D, R2))
    ((White, Pawn  ), (E, R2))
    ((White, Pawn  ), (F, R2))
    ((White, Pawn  ), (G, R2))
    ((White, Pawn  ), (H, R2))
    ((Black, Pawn  ), (A, R7))
    ((Black, Pawn  ), (B, R7))
    ((Black, Pawn  ), (C, R7))
    ((Black, Pawn  ), (D, R7))
    ((Black, Pawn  ), (E, R7))
    ((Black, Pawn  ), (F, R7))
    ((Black, Pawn  ), (G, R7))
    ((Black, Pawn  ), (H, R7))
    ((Black, Rook  ), (A, R8))
    ((Black, Knight), (B, R8))
    ((Black, Bishop), (C, R8))
    ((Black, Queen ), (D, R8))
    ((Black, King  ), (E, R8))
    ((Black, Bishop), (F, R8))
    ((Black, Knight), (G, R8))
    ((Black, Rook  ), (H, R8))
]

let pieceToString (color, chessman) = sprintf "%A%A" color chessman

let board game =
    Array2D.init 8  8 (fun r f ->
        game
        |> at (File.fromInt f, Rank.fromInt r)
        |> Option.map piece
        |> Option.map pieceToString
        |> defaultArg2 "  ")

init
|> moveAndReplace ((White, Pawn), (E, R2)) (E, R4)
|> moveAndReplace ((Black, Pawn), (D, R7)) (E, R5)
|> board


//|> List.map (fun cell -> (cell, capabilities init (position cell)))
//|> Seq.toList
//|> printfn "%A"

knightReaches  (B, R1)
capabilities game2 (A, R1) |> Seq.toList |> printfn "%A"
// UTILS
module Option =
    let defaultTo defValue opt = defaultArg opt defValue
    let pipe action = Option.map (fun x -> action x; x)
    let mapList fn opt =
        opt
        |> Option.map fn 
        |> defaultTo []

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
type Color = Black | White with
    member this.toString =
        match this with
        | Black -> "b"
        | White -> "w"

type Piece = Piece of Color * Chessman

type CellState = 
    | Cell of Piece * Position
    | Empty of Position

let cell color chessman pos = (Piece (color, chessman), pos)

let isEmpty = function
    | Empty _ -> true
    | _ -> false

type GameState = (Piece * Position) list

let color = function
    | Cell (Piece (col, _), _) -> Some col
    | _ -> None

let fcolor (Piece (color, _), _) = color

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

let sameColor cell1 cell2 =
    color cell1 = color cell2

let evaluateSquareAction game targetPos sourceCell action =
    let targetCell = game @ targetPos
    match action with
    | Move ->
        targetCell |> isEmpty
    | Capture ->
        targetCell |> (not << sameColor sourceCell) &&
        targetCell |> (not << isEmpty)
    | EnPassant ->
        isEmpty targetCell

let is x = function
    | x -> true
    | _ -> false

let reachIsBlocked = function
    | Some Move -> true
    | _ -> false

let evaluateSquare game actionsToAnalyze sourceCell targetPos =
    actionsToAnalyze
    |> Seq.filter (evaluateSquareAction game targetPos sourceCell)
    |> Seq.map (fun action -> (action, targetPos))
    |> Seq.tryHead

let reachCapabilities game sourceCell actionsToAnalyze reach =
    reach
    |> Seq.map (evaluateSquare game actionsToAnalyze sourceCell)
    |> Seq.takeWhilePlusOne (is (Some Move))
    |> Seq.filterNones

let reachesCapabilities (game: GameState) (cellToMove: CellState) (reachesGenerator, actionsToAnalyze) =
    cellToMove
    |> position
    |> reachesGenerator
    |> Seq.collect (reachCapabilities game cellToMove actionsToAnalyze)

let pieceCapabilities game cellToMove =
    cellToMove
    |> piece
    |> Option.map pieceReachesGenerators
    |> defaultArg <| []
    |> Seq.collect (reachesCapabilities game cellToMove)

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacksBy game cellState =
    pieceCapabilities game cellState
    |> Seq.filter (fun (action, pos) -> action = Capture)
    |> Seq.map snd

/// <summary>Determines the attacks a piece of the game is making.</summary>
let attacks col game = 
    game
    |> Seq.filter (fun cell -> fcolor cell = col)
    |> Seq.map Cell
    |> Seq.collect (attacksBy game >> Seq.toList) 

let isAttacked color game targetPosition =
    game
    |> attacks color
    |> Seq.contains targetPosition
    
let isCheck color game =
    game
    |> Seq.tryFind (fun (piece, pos) -> piece = Piece (color, King))
    |> Option.map (fun (piece, pos) -> pos)
    |> Option.map (isAttacked color game)
    |> defaultArg <| false

let moveAndReplace sourcePos (targetPos: Position) game =
    match game @ sourcePos with
    | Empty _ -> List.empty
    | Cell (piece, _) -> (piece, targetPos) :: (game |> List.filter (fun (_, position) -> position <> sourcePos && position <> targetPos))

let capabilities game pos =
    match game @ pos with
    | Empty _ -> Seq.empty
    | Cell (piece, sourcePos) as cellState ->
        let capabilitiesFilter2 (_, targetPos) =
            game
            |> moveAndReplace pos targetPos
            |> (not << isCheck (fcolor (piece, sourcePos)))
        cellState
        |> pieceCapabilities game
        |> Seq.debug "capabilityBeforeCheckTest"
        |> Seq.filter capabilitiesFilter2


let init: GameState = [
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

// OUTPUT

let positionToString ((f, r):Position) =
    sprintf "%A%A" f r

let printPositions =
    List.map positionToString >> List.iter (printf "%A")

let cellStateToString = function
    | Empty _ -> "  "
    | Cell (Piece (color, chessman), _) -> sprintf "%A%A" color chessman

let toMatrix game =
    Array2D.init 8  8 (fun r f ->
        game @ (File.fromInt f, Rank.fromInt r)
    )

let board toString game =
    let stringMatrix = toMatrix game |> Array2D.map toString

    [|0..7|]
    |> Array.map (fun i -> stringMatrix.[i, *])
    |> Array.map (String.concat "|")

let printBoard b = 
    let printRow i x = printfn "%i|%s|%i" (8-i) x (8-i)
    
    let filesHeader =
        [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"|]
        |> Array.map (fun x -> x.PadLeft(4, ' '))
        |> String.concat ""

    printfn "%s" filesHeader
    b |> Array.iteri printRow
    printfn "%s" filesHeader

let cellWithCaps capList cellState =
    let capabilityAt pos = capList |> Seq.tryFind (fun (act, p) -> p = pos)

    let actionToString = function
        | Move -> "m"
        | Capture -> "c"
        | EnPassant -> "p"

    let cellStateActionToString =
        cellState
        |> position
        |> capabilityAt
        |> Option.map fst
        |> Option.map actionToString
        |> defaultArg <| " "

    (cellStateToString cellState) + cellStateActionToString

// TESTING
let game1: GameState = [
    cell White Bishop (A, R1)
]

let game2: GameState = [
    cell White Bishop (A, R1)
    cell White Knight (C, R3)
]

let game3 = 
    init
    |> moveAndReplace (E, R2) (E, R4)
    |> moveAndReplace (C, R8) (H, R3)

game3 |> board cellStateToString |> printBoard
game3 |> board (cellWithCaps (capabilities game3 (H, R3))) |> printBoard

game3
|> List.map (fun (piece, pos) -> ((piece, pos), capabilities game3 pos))
|> Seq.toList
|> ignore

//|> List.map (fun cell -> (cell, capabilities init (position cell)))
//|> Seq.toList
//|> printfn "%A"

knightReaches  (B, R1)
capabilities game2 (A, R1) |> Seq.toList |> printfn "%A"
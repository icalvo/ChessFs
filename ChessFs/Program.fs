type Chessman =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King

type File = A | B | C | D | E | F | G | H

type Rank = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8

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

let sameRank = Some
let sameFile = Some

type Position = File * Rank

let simplifyTuple = function
    | (Some a, Some b) -> Some (a, b)
    | _ -> None

let move rankFn fileFn (a, b) = (rankFn a, fileFn b) |> simplifyTuple

type Movement = Position -> Position option

let Up       : Movement = move nextRank sameFile
let UpRight  : Movement = move nextRank nextFile
let Right    : Movement = move sameRank nextFile
let DownRight: Movement = move prevRank nextFile
let Down     : Movement = move prevRank sameFile
let DownLeft : Movement = move prevRank prevFile
let Left     : Movement = move sameRank prevFile
let UpLeft   : Movement = move nextRank prevFile

let extend (moveFn: Movement) =
    moveFn >> Option.map (fun x -> (x, x)) |> Seq.unfold

let Listapply arg fnseq = fnseq |> List.map (fun fn -> fn arg)

let bishopExtensions pos =
    [ UpRight; DownRight; DownLeft; UpLeft ]
    |> List.map extend
    |> Listapply pos

let rookExtensions pos =
    [ Up; Right; Down; Left ]
    |> List.map extend
    |> Listapply pos

let queenExtensions pos =
    [ bishopExtensions; rookExtensions ]
    |> Listapply pos
    |> List.concat

let kingExtensions pos =
    [ UpRight; DownRight; DownLeft; UpLeft; Up; Right; Down; Left ]
    |> Listapply pos
    |> List.choose id
    |> List.map Seq.singleton

let (>?>) f1 f2 = f1 >> Option.bind f2

let (|?>) x f2 = Some x |> Option.bind f2

let knightExtensions pos =
    [
        Up >?> Left >?> Up;
        Up >?> Left >?> Left;
        Down >?> Left >?> Left;
        Down >?> Left >?> Left;
    ]
    |> Listapply pos
    |> List.choose id
    |> List.map Seq.singleton

let pawnMoveExtensions rankForDoubleMove moveFn pos =
    match pos with
    | (_, rank) when rank = rankForDoubleMove ->
        extend moveFn pos
        |> Seq.take 2
        |> List.singleton
    | _ ->
        extend moveFn pos
        |> Seq.take 1
        |> List.singleton

let pawnCaptureExtensions moveFn pos =
    [
        moveFn >?> Left;
        moveFn >?> Right;
    ]
    |> Listapply pos
    |> List.choose id
    |> List.map Seq.singleton

let pawnEnPassantExtensions (rankForEnPassant: Rank) moveFn (pos: Position) =
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

type ColourChessman = Colour * Chessman

type CellState = ColourChessman * Position

type GameState = CellState list

let at (game: GameState) (pos: Position): CellState option =
    List.tryFind (fun (ccm, p) -> p = pos) game

let game1: GameState = [
    ((White, Bishop), (A, F1))
]

type Action =
    | Move
    | Capture
    | EnPassant

let colouredChessmanExtensions = function
    | (White, Pawn) ->
        [
            (pawnMoveExtensions F2 Up, [ Move; ]);
            (pawnCaptureExtensions Up, [ Capture; ]);
            (pawnEnPassantExtensions F5 Up, [ EnPassant; ])
        ]
    | (Black, Pawn) ->
        [
            (pawnMoveExtensions F7 Down, [ Move; ]);
            (pawnCaptureExtensions Down, [ Capture; ]);
            (pawnEnPassantExtensions F4 Down, [ EnPassant; ])
        ]
    | (_, Knight) -> [(knightExtensions, [ Move; Capture ])]
    | (_, Bishop) -> [(bishopExtensions, [ Move; Capture ])]
    | (_, Rook) -> [(rookExtensions, [ Move; Capture ])]
    | (_, Queen) -> [(queenExtensions, [ Move; Capture ])]
    | (_, King) -> [(kingExtensions, [ Move; Capture ])]

let sameColor (cell1: CellState) (cell2: CellState) =
    fst (fst cell1) = fst (fst cell2)

let evaluateSquareAction game target cellToMove action =
    let targetCell = at game target
    let filteredTarget =
        match action with
        | Move ->
            match targetCell with
            | Some _ -> None
            | None -> Some target
        | Capture ->
            targetCell
            |> Option.filter (sameColor cellToMove) 
            |> Option.map (fun _ -> target)
        | EnPassant -> None
    filteredTarget |> Option.map (fun tgt -> (action, tgt))

let evaluateSquare game actionsToAnalyze cellToMove target =
    actionsToAnalyze
    |> List.map (evaluateSquareAction game target cellToMove)
    |> List.choose id
    |> List.tryHead

let extensionCapabilities game cellToMove actionsToAnalyze targets =
    targets
    |> Seq.map (evaluateSquare game actionsToAnalyze cellToMove)
    |> Seq.choose id
    |> Seq.toList

let extensionListCapabilities (game: GameState) cellToMove (extfn, actionsToAnalyze) =
    let extensions: seq<Position> list = extfn (snd cellToMove)
    extensions
    |> List.collect (extensionCapabilities game cellToMove actionsToAnalyze)


let colouredChessmanCapabilities (game: GameState) (cellToMove: CellState) =
    cellToMove
    |> fst
    |> colouredChessmanExtensions
    |> List.collect (extensionListCapabilities game cellToMove)

let capabilities game pos =
    let cellToMove = at game pos
    cellToMove
    |> Option.map (colouredChessmanCapabilities game)


let positionToString (r, f) =
    sprintf "%A%i" r (fileToInt f)


let printPositions (l: Position list) =
    l |> List.map positionToString |> List.iter (printf "%A")

printfn "%A" (capabilities game1 (A, F1))

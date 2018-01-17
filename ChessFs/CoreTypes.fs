module CoreTypes

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
    member x.toInt =
        match x with
        | R1 -> 7
        | R2 -> 6
        | R3 -> 5
        | R4 -> 4
        | R5 -> 3
        | R6 -> 2
        | R7 -> 1
        | R8 -> 0

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
    member x.toInt =
        match x with
        | A -> 0
        | B -> 1
        | C -> 2
        | D -> 3
        | E -> 4
        | F -> 5
        | G -> 6
        | H -> 7

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
    | (f, R1) -> Some (f, R2)
    | (f, R2) -> Some (f, R3)
    | (f, R3) -> Some (f, R4)
    | (f, R4) -> Some (f, R5)
    | (f, R5) -> Some (f, R6)
    | (f, R6) -> Some (f, R7)
    | (f, R7) -> Some (f, R8)
    | (f, R8) -> None



let prevRank = function
    | (f, R1) -> None
    | (f, R2) -> Some (f, R1)
    | (f, R3) -> Some (f, R2)
    | (f, R4) -> Some (f, R3)
    | (f, R5) -> Some (f, R4)
    | (f, R6) -> Some (f, R5)
    | (f, R7) -> Some (f, R6)
    | (f, R8) -> Some (f, R7)

let nextFile = function
    | (A, r) -> Some (B, r)
    | (B, r) -> Some (C, r)
    | (C, r) -> Some (D, r)
    | (D, r) -> Some (E, r)
    | (E, r) -> Some (F, r)
    | (F, r) -> Some (G, r)
    | (G, r) -> Some (H, r)
    | (H, r) -> None

let prevFile = function
    | (A, r) -> None
    | (B, r) -> Some (A, r)
    | (C, r) -> Some (B, r)
    | (D, r) -> Some (C, r)
    | (E, r) -> Some (D, r)
    | (F, r) -> Some (E, r)
    | (G, r) -> Some (F, r)
    | (H, r) -> Some (G, r)

type Position = File * Rank

type Color = Black | White

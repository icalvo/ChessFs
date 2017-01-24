module Output

open Domain

[<StructuredFormatDisplay("{toString}")>]
type Chessman
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
type Rank
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

[<StructuredFormatDisplay("{toString}")>]
type Color with
    member this.toString =
        match this with
        | Black -> "b"
        | White -> "w"

let positionToString ((f, r):Position) =
    sprintf "%A%A" f r

let printPositions =
    List.map positionToString >> List.iter (printf "%A")

let cellStateToString = function
    | Empty _ -> "  "
    | Cell (Piece (color, chessman), _) -> sprintf "%A%A" color chessman

let board toString displayInfo =
    let stringMatrix = displayInfo |> Array2D.map toString

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

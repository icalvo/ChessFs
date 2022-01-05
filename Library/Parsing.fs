module ChessParsing

open FParsec
open CoreTypes
open Chess
open Utils

let ppiece = choice [
    charReturn 'K' WhiteKing
    charReturn 'Q' WhiteQueen
    charReturn 'R' WhiteRook
    charReturn 'B' WhiteBishop
    charReturn 'N' WhiteKnight
    charReturn 'P' WhitePawn
    charReturn 'k' BlackKing
    charReturn 'q' BlackQueen
    charReturn 'r' BlackRook
    charReturn 'b' BlackBishop
    charReturn 'n' BlackKnight
    charReturn 'p' BlackPawn
]

let pplayer = choice [
    charReturn 'w' White
    charReturn 'b' Black
]

let pcr c = c |> pchar |> opt |>> Option.isSome
let pcastling = pipe4 (pcr 'K') (pcr 'Q') (pcr 'k') (pcr 'q') (fun wk wq bk bq ->
    {
        canCastleKingSide = wk
        canCastleQueenSide = wq
    },
    {
        canCastleKingSide = bk
        canCastleQueenSide = bq
    })

let pemptySquareNumber = (anyOf "12345678") |>> (sprintf "%c" >> int >> List.repeat None)
let pieceSquareRange x = [Some x]
let emptySquareRange n = List.init n (fun _ -> None)
let psquare = (ppiece |>> pieceSquareRange) <|> pemptySquareNumber
let prow = (many psquare |>> List.concat)

let toSquare r f piece = ((File.fromInt f, Rank.fromInt r), piece)

let toMap ll =
    let lengthIs8 l = List.length l = 8
    if (ll |> lengthIs8 |> not) then
        fail $"There are %i{List.length ll} rows and there should be 8"
    elif List.exists (lengthIs8 >> not) ll then
        let index = 1 + List.findIndex (not << lengthIs8) ll
        fail $"There are some rows that have not 8 columns; the first one is #%i{index} with %i{List.item index ll |> List.length} columns"
    else
        ll
        |> List.mapi (fun r -> List.mapi (fun f -> Option.map (fun piece -> ((File.fromInt f, Rank.fromInt r), piece))))
        |> List.concat
        |> List.filterNones
        |> Map.ofList
        |> preturn

let pboard = sepBy prow (pchar '/') >>= toMap

let pfile = choice [
    charReturn 'a' A
    charReturn 'b' B
    charReturn 'c' C
    charReturn 'd' D
    charReturn 'e' E
    charReturn 'f' F
    charReturn 'g' G
    charReturn 'h' H
]

let prank = (anyOf "12345678") |>> (sprintf "%c" >> int >> (fun x -> x - 1) >> Rank.fromInt)

let pcoord = tuple2 pfile prank  |>> Some
let penpassant = choice [
    charReturn '-' None
    pcoord ]

let pstatus = tuple5 (pplayer .>> pchar ' ') (pcastling .>> pchar ' ') (penpassant .>> pchar ' ') (pint32 .>> pchar ' ') pint32
let pfen = pboard .>> (pchar ' ') .>>. pstatus |>> (fun (board, (player, (wcr, bcr), enpassant, n1, n2)) -> {
    pieces = board
    playerInTurn = player
    whitePlayerCastlingRights = wcr
    blackPlayerCastlingRights = bcr
    pawnCapturableEnPassant = enpassant
    pliesWithoutPawnOrCapture = n1
    plies = []
    numberOfMoves = n2
    repeatableStates = []
})


let parseFEN = run pfen

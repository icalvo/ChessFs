module ChessParsing

open FParsec
open Utils
open CoreTypes
open Engine
open Notation

let charChoice list = choice (List.map (fun (ch, res) -> charReturn ch res) list)

let ppiece = charChoice [
    'K', WhiteKing
    'Q', WhiteQueen
    'R', WhiteRook
    'B', WhiteBishop
    'N', WhiteKnight
    'P', WhitePawn
    'k', BlackKing
    'q', BlackQueen
    'r', BlackRook
    'b', BlackBishop
    'n', BlackKnight
    'p', BlackPawn
]

let pplayer = charChoice [
    'w', Player White
    'b', Player Black
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

let pnumberOneToEight = charChoice [
    '1', 1
    '2', 2
    '3', 3
    '4', 4
    '5', 5
    '6', 6
    '7', 7
    '8', 8
]

let pemptySquareNumber = pnumberOneToEight |>> List.repeat None
let pieceSquareRange x = [Some x]
let emptySquareRange n = List.init n (fun _ -> None)
let psquare = (ppiece |>> pieceSquareRange) <|> pemptySquareNumber
let prow = (many psquare |>> List.concat)

let toMap ll =
    let lengthIs8 l = List.length l = 8
    if (ll |> lengthIs8 |> not) then
        failFatally $"There are %i{List.length ll} rows and there should be 8"
    elif List.exists (lengthIs8 >> not) ll then
        let index = 1 + List.findIndex (not << lengthIs8) ll
        failFatally $"There are some rows that have not 8 columns; the first one is #%i{index} with %i{List.item index ll |> List.length} columns"
    else
        let files = [A; B; C; D; E; F; G; H]
        let ranks = [
            R8
            R7
            R6
            R5
            R4
            R3
            R2
            R1
        ]        
        ll
        |> List.mapi (fun r -> List.mapi (fun f -> Option.map (fun piece -> ((files.[f], ranks.[r]), piece))))
        |> List.concat
        |> List.filterNones
        |> Map.ofList
        |> preturn

let pboard = sepBy prow (pchar '/') >>= toMap

let pfile = charChoice [
    'a', A
    'b', B
    'c', C
    'd', D
    'e', E
    'f', F
    'g', G
    'h', H
]


let prank = charChoice [
    '1', R1
    '2', R2
    '3', R3
    '4', R4
    '5', R5
    '6', R6
    '7', R7
    '8', R8
]

let pcoord = tuple2 pfile prank |>> Some
let penpassant = choice [
    charReturn '-' None
    pcoord ]

let pstatus = tuple5 (pplayer .>> pchar ' ') (pcastling .>> pchar ' ') (penpassant .>> pchar ' ') (pint32 .>> pchar ' ') pint32
let pfen = pboard .>> (pchar ' ') .>>. pstatus |>> (fun (board, (player, (wcr, bcr), enpassant, ppc, nm)) -> {
    pieces = board
    playerInTurn = player
    whitePlayerCastlingRights = wcr
    blackPlayerCastlingRights = bcr
    pawnCapturableEnPassant = enpassant
    pliesWithoutPawnOrCapture = ppc
    numberOfMoves = nm
})

let parseFEN = run pfen


module ChessState =
    let fromFEN str =
        let parse = parseFEN str
        match parse with
        | Success (r, _, _) -> ChessState.fromRaw r
        | Failure (s, _, _) -> failwith s

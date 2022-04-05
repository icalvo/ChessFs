namespace ChessFs.Chess

open ChessFs.Chess

type ParsedPly =
    | Move of Shape * Coordinate
    | Capture of Shape * Coordinate
    | CastleKingSide of Color
    | CastleQueenSide of Color
    | MoveAndPromote of Color * Coordinate * Shape
    | CaptureAndPromote of Color * Coordinate * Shape

type ParsedPlayerAction =
    | MovePiece of ParsedPly * DrawOfferStatus
    | Resign
    | AcceptDraw

module ParsedPlayerAction =
    let matches (action: PlayerAction) (parsed: ParsedPlayerAction) =
        match action, parsed with
        | PlayerAction.AcceptDraw, AcceptDraw
        | PlayerAction.Resign, Resign -> true
        | PlayerAction.MovePiece (ply, _, dos), MovePiece (parsedPly, dos2) ->
            dos = dos2 &&
            match ply, parsedPly with
            | Ply.Move (Piece (_, shape), _, t), Move (shape2, t2) -> (shape, t) = (shape2, t2)
            | Ply.Capture (Piece(_, shape), _, t), Capture (shape2, t2) -> (shape, t) = (shape2, t2)
            | Ply.CaptureEnPassant (_, _, t), Capture (Pawn, t2) -> t = t2
            | Ply.CastleKingSide color, CastleKingSide color2 -> color = color2
            | Ply.CastleQueenSide color, CastleQueenSide color2 -> color = color2
            | Ply.MoveAndPromote (color, _, t, shape), MoveAndPromote (color2, t2, shape2) -> (color, shape, t) = (color2, shape2, t2)
            | Ply.CaptureAndPromote (color, _, t, shape), CaptureAndPromote (color2, t2, shape2) -> (color, shape, t) = (color2, shape2, t2)
            | _ -> false
        | _ -> false

module Parsing =
    open FParsec
    open ChessFs.Common
    open ChessFs.Chess.Engine
    open Piece
    open Notation
    open Setup

    let private charChoice list = choice (List.map (fun (ch, res) -> charReturn ch res) list)

    let private pshape = charChoice [
        'K', King
        'Q', Queen
        'R', Rook
        'B', Bishop
        'N', Knight
    ]

    let private ppiece = charChoice [
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

    let private pplayer = charChoice [
        'w', Player White
        'b', Player Black
    ]

    let private pcr c = c |> pchar |> opt |>> Option.isSome
    let private pcastling = pipe4 (pcr 'K') (pcr 'Q') (pcr 'k') (pcr 'q') (fun wk wq bk bq ->
        {
            canCastleKingSide = wk
            canCastleQueenSide = wq
        },
        {
            canCastleKingSide = bk
            canCastleQueenSide = bq
        })

    let private pnumberOneToEight = charChoice [
        '1', 1
        '2', 2
        '3', 3
        '4', 4
        '5', 5
        '6', 6
        '7', 7
        '8', 8
    ]

    let private pemptySquareNumber = pnumberOneToEight |>> List.repeat None
    let private pieceSquareRange x = [Some x]
    let private emptySquareRange n = List.init n (fun _ -> None)
    let private psquare = (ppiece |>> pieceSquareRange) <|> pemptySquareNumber
    let private prow = (many psquare |>> List.concat)

    let private toMap ll =
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

    let private pboard = sepBy prow (pchar '/') >>= toMap

    let private pfile = charChoice [
        'a', A
        'b', B
        'c', C
        'd', D
        'e', E
        'f', F
        'g', G
        'h', H
    ]


    let private prank = charChoice [
        '1', R1
        '2', R2
        '3', R3
        '4', R4
        '5', R5
        '6', R6
        '7', R7
        '8', R8
    ]

    let private pcoord = tuple2 pfile prank
    let private penpassant = choice [
        charReturn '-' None
        pcoord |>> Some ]

    let private pstatus = tuple5 (pplayer .>> pchar ' ') (pcastling .>> pchar ' ') (penpassant .>> pchar ' ') (pint32 .>> pchar ' ') pint32
    let private pfen = pboard .>> (pchar ' ') .>>. pstatus |>> (fun (board, (player, (wcr, bcr), enpassant, ppc, nm)) -> {
        pieces = board
        playerInTurn = player
        castlingRights = {
            white = wcr
            black = bcr
        }
        pawnCapturableEnPassant = enpassant
        pliesWithoutPawnOrCapture = ppc
        numberOfMoves = nm
    })

    let pAcceptDraw = stringReturn ":a" AcceptDraw
    let pResign = stringReturn ":r" Resign
    let pDrawOfferStatus = opt (pstring ":d") |>> function | Some _ -> IsDrawOffer | _ -> NoDrawOffer

    open Coordinate

    let trol (shapeOpt, _, _, (captOpt, coord), promOpt, dos) =
        let ply =
            match shapeOpt with
            | Some shape ->
                match captOpt with
                | Some _ -> Capture (shape, coord)
                | None -> Move (shape, coord)
            | None ->
                let shape = Pawn
                match promOpt, captOpt with
                | Some prom, Some _ -> CaptureAndPromote(White, coord, prom)
                | Some prom, None -> MoveAndPromote(White, coord, prom)
                | _, Some _ -> Capture(shape, coord)
                | _ -> Move (shape, coord)

        MovePiece(ply, dos)

    let tuple6 a1 a2 a3 a4 a5 a6 = tuple5 a1 a2 a3 a4 a5 .>>. a6 |>> fun (((x1, x2, x3, x4, x5), x6)) -> (x1, x2, x3, x4, x5, x6)
    let tuple7 a1 a2 a3 a4 a5 a6 a7 = tuple6 a1 a2 a3 a4 a5 a6 .>>. a7 |>> fun (((x1, x2, x3, x4, x5, x6), x7)) -> (x1, x2, x3, x4, x5, x6, x7)

    let pcoordsRange = choice [
        pfile >>. prank >>. (opt (pchar 'x')) .>>. pcoord
        pfile >>. prank >>. preturn None .>>. pcoord
        pfile >>. (opt (pchar 'x')) .>>. pcoord
        prank >>. (opt (pchar 'x')) .>>. pcoord
        pfile >>. preturn None .>>. pcoord
        prank >>. preturn None .>>. pcoord
        (opt (pchar 'x')) .>>. pcoord
        preturn None .>>. pcoord
    ]

    let ppromotionShape = pstring "=" >>. pshape
    let pMovePiece = tuple6 (opt pshape) (opt pfile) (opt prank) pcoordsRange (opt ppromotionShape) pDrawOfferStatus |>> trol

    let private pplayerAction = choice [ pAcceptDraw; pResign; pMovePiece ]

    let parsePlayerAction str =
        let parsed = run pplayerAction str

        match parsed with
        | Success (p, _ , _) -> Result.Ok p
        | Failure (s, _ , _) -> Result.Error (Parsing s)

    let parseFEN = run pfen

    let fromFEN str =
        let parse = parseFEN str
        match parse with
        | Success (r, _, _) -> ChessState.fromRaw r
        | Failure (s, _, _) -> Result.Error (Parsing s)

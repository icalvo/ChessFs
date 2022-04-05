namespace ChessFs.Chess

open ChessFs.Chess

type ParsedPly =
    | Move of Shape * Coordinate
    | Capture of Shape * Coordinate
    | CastleKingSide
    | CastleQueenSide
    | MoveAndPromote of Coordinate * Shape
    | CaptureAndPromote of Coordinate * Shape

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
            | Ply.CastleKingSide _, CastleKingSide -> true
            | Ply.CastleQueenSide _, CastleQueenSide -> true
            | Ply.MoveAndPromote (color, _, t, shape), MoveAndPromote (t2, shape2) -> (shape, t) = (shape2, t2)
            | Ply.CaptureAndPromote (color, _, t, shape), CaptureAndPromote (t2, shape2) -> (shape, t) = (shape2, t2)
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

    let trol (shapeOpt, (isCapture, target), promOpt, dos) =
        let ply =
            match shapeOpt with
            | Some shape ->
                if isCapture then
                    Capture (shape, target)
                else
                    Move (shape, target)
            | None ->
                let shape = Pawn
                match promOpt, isCapture with
                | Some prom, true -> CaptureAndPromote (target, prom)
                | Some prom, false -> MoveAndPromote (target, prom)
                | _, true -> Capture (shape, target)
                | _ -> Move (shape, target)

        MovePiece(ply, dos)

    let pcoordsRange = choice [
        attempt (opt pfile >>. opt prank >>. (opt (pchar 'x') |>> Option.isSome) .>>. pcoord)
        preturn None >>. preturn None >>. preturn false .>>. pcoord 
    ]

    let ppromotionShape = pstring "=" >>. pshape
    let pMovePiece = choice [
        stringReturn "O-O-O:d" (ParsedPlayerAction.MovePiece (ParsedPly.CastleQueenSide, IsDrawOffer))
        stringReturn "O-O-O" (ParsedPlayerAction.MovePiece (ParsedPly.CastleQueenSide, NoDrawOffer))
        stringReturn "O-O:d" (ParsedPlayerAction.MovePiece (ParsedPly.CastleKingSide, IsDrawOffer))
        stringReturn "O-O" (ParsedPlayerAction.MovePiece (ParsedPly.CastleKingSide, NoDrawOffer))
        tuple4 (opt pshape) pcoordsRange (opt ppromotionShape) pDrawOfferStatus |>> trol
    ]

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

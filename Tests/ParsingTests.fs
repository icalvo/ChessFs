namespace ChessParsing

open ChessFs.Chess

module ``FEN Parsing`` =
    open FParsec
    open Xunit
    open Swensen.Unquote
    open ChessFs.Chess.Engine
    open Parsing
    open StateMachine

    let resultOrFail =
        function
        | Success (r, _, _) -> r
        | Failure (msg, _, _) -> failwith msg

    [<Fact>]
    let ``Initial state``() =
        let actual = parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        match actual with
        | Success (r, _, _) -> r =! Setup.initial.toRaw
        | Failure (msg, _, _) -> failwith msg

    [<Fact>]
    let ``After 1.e4 e5 state``() =
        let expected = ChessState.stateAfter [ "e4"; "e5" ]
        let actual = parseFEN "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"
        match actual with
        | Success (r, _, _) -> r =! expected.toRaw
        | Failure (msg, _, _) -> failwith msg

//    open Coordinate
//
//    [<Fact>]
//    let ``sdfdsf`` =
//        ":a" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.AcceptDraw)
//        ":r" |> parsePlayerAction =! Result.Ok ParsedPlayerAction.Resign
//        "e4" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Pawn, E4), NoDrawOffer))
//        "e4:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Pawn, E4), IsDrawOffer))
//        "Qxf3" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Capture (Queen, F3), NoDrawOffer))

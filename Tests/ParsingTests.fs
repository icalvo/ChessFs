namespace ChessParsing

open ChessFs.Chess

module ``FEN Parsing`` =
    open FParsec
    open Xunit
    open Swensen.Unquote
    open ChessFs.Chess.Engine
    open Parsing
    open StateMachine
    open Coordinate


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

    [<Fact>]
    let ``Player action parsing`` () =
        ":a" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.AcceptDraw)
        ":r" |> parsePlayerAction =! Result.Ok ParsedPlayerAction.Resign
        "e4" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Pawn, E4), NoDrawOffer))
        "Nf3" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Knight, F3), NoDrawOffer))
        "N1f3" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Knight, F3), NoDrawOffer))
        "Nef3" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Knight, F3), NoDrawOffer))
        "Ne1f3" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Knight, F3), NoDrawOffer))
        "N1xf3" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Capture (Knight, F3), NoDrawOffer))
        "Nexf3" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Capture (Knight, F3), NoDrawOffer))
        "Ne1xf3" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Capture (Knight, F3), NoDrawOffer))
        "e1=Q" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.MoveAndPromote (E1, Queen), NoDrawOffer))
        "exf8=N" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.CaptureAndPromote (F8, Knight), NoDrawOffer))
        "e4:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Pawn, E4), IsDrawOffer))
        "Nf3:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Knight, F3), IsDrawOffer))
        "N1f3:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Knight, F3), IsDrawOffer))
        "N1f3:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Knight, F3), IsDrawOffer))
        "Nef3:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Knight, F3), IsDrawOffer))
        "Ne1f3:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Move (Knight, F3), IsDrawOffer))
        "N1xf3:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Capture (Knight, F3), IsDrawOffer))
        "Nexf3:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Capture (Knight, F3), IsDrawOffer))
        "Ne1xf3:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.Capture (Knight, F3), IsDrawOffer))
        "e1=Q:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.MoveAndPromote (E1, Queen), IsDrawOffer))
        "exf8=N:d" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.CaptureAndPromote (F8, Knight), IsDrawOffer))
        "O-O" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.CastleKingSide, NoDrawOffer))
        "O-O-O" |> parsePlayerAction =! Result.Ok (ParsedPlayerAction.MovePiece (ParsedPly.CastleQueenSide, NoDrawOffer))

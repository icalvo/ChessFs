namespace ChessParsing

module ``FEN Parsing`` =
    open FParsec
    open Xunit
    open Swensen.Unquote
    open Engine
    open ChessParsing
    open ChessStateMachine

    let resultOrFail =
        function
        | Success (r, _, _) -> r
        | Failure (msg, _, _) -> failwith msg

    [<Fact>]
    let ``Initial state``() =
        let actual = parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        match actual with
        | Success (r, _, _) -> r =! ChessState.initial.toRaw
        | Failure (msg, _, _) -> failwith msg

    [<Fact>]
    let ``After 1.e4 e5 state``() =
        let coord = run pcoord "e6" |> resultOrFail
        coord =! Some CoreTypes.E6
        let expected = ChessState.stateAfter [ "e4"; "e5" ]
        let actual = parseFEN "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"
        match actual with
        | Success (r, _, _) -> r =! expected.toRaw
        | Failure (msg, _, _) -> failwith msg

namespace ChessParsing

module ``FEN Parsing`` =
    open FParsec
    open Xunit
    open Swensen.Unquote
    open Chess
    open ChessParsing

    let resultOrFail =
        function
        | Success (r, _, _) -> r
        | Failure (msg, _, _) -> failwith msg

    [<Fact>]
    let ``Initial state``() =
        let actual = parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        match actual with
        | Success (r, _, _) -> r =! initialGameState
        | Failure (msg, _, _) -> failwith msg

namespace ChessFs.Tests

module ``transitions`` =
    open Xunit
    open Swensen.Unquote
    open Chess
    open Notation
    open ChessStateMachine

    //let findExecutableAction (input:string) availableActions =
    //     availableActions
    //     |> List.filter (fun { action = m } -> (playerActionToAlgebraic m).ToLowerInvariant() = input.ToLowerInvariant())
    //     |> List.tryHead

    let equality m (input:string) = (playerActionToAlgebraic m).ToLowerInvariant() = input.ToLowerInvariant()

    [<Fact>]
    let ``Stalemate``() =
        
                ["e3"; "a5";"Qh5";"Ra6";"Qxa5";"h5";"h4";"Rah6";"Qxc7";"f6";"Qxd7";"Kf7";"Qxb7";"Qd3"; "Qxb8";"Qh7"; "Qxc8"; "Kg6"; "Qe6"]
                |> algebraicChessStateMachine
                |> Seq.last
                |> outcomeToPGN =! "1. e3 a5 2. Qh5 Ra6 3. Qxa5 h5 4. h4 Rah6 5. Qxc7 f6 6. Qxd7+ Kf7 7. Qxb7 Qd3 8. Qxb8 Qh7 9. Qxc8 Kg6 10. Qe6 1/2-1/2 {Stalemate}"

    [<Fact>]
    let ``Fool's mate``() =
        test <@
                [ "f3"; "e5"; "g4"; "Qh4" ]
                |> algebraicChessStateMachine
                |> Seq.last
                |> outcomeToPGN = "1. f3 e5 2. g4 Qh4# 0-1"
        @>

    [<Fact>]
    let ``Draw by agreement``() =
        test <@
                [ "f3"; ":d"; ":d"; "e5"; ":d"; ":a" ]
                |> algebraicChessStateMachine
                |> Seq.last
                |> outcomeToPGN = "1. f3 e5 1/2-1/2 {Agreement}"
        @>

    [<Fact>]
    let ``Draw declined and accepted``() =
        
                [ "f3"; ":d"; ":d"; "e5"; ":d"; ":a" ]
                |> algebraicChessStateMachine
                |> Seq.map (fun x -> $"%s{outcomeToPGN x} {{%s{x.displayInfo.gameState.statusToFEN}}}")
                |> Seq.toList =! [
                " {w KQkq - 0 1}";
                "1. f3 {b KQkq - 0 1}";
                "1. f3 {Draw offered} {w KQkq - 0 1}";
                "1. f3 {Draw declined} {b KQkq - 0 1}";
                "1. f3 e5 {w KQkq e6 0 2}";
                "1. f3 e5 {Draw offered} {b KQkq e6 0 2}";
                "1. f3 e5 1/2-1/2 {Agreement} {b KQkq e6 0 2}"]

    [<Fact>]
    let ``After three-fold repetition, a draw offer is automatically accepted``() =
        let outcomes =
            [ "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; ":d"; ]
            |> algebraicChessStateMachine
        test <@
                outcomes
                |> Seq.last
                |> outcomeToPGN = "1. Nc3 Nc6 2. Nb1 Nb8 3. Nc3 Nc6 4. Nb1 Nb8 5. Nc3 Nc6 6. Nb1 Nb8 1/2-1/2 {ThreefoldRepetition}"
        @>

    [<Fact>]
    let ``After three-fold repetition in the past, a draw offer is NOT automatically accepted``() =
        let outcomes =
            [ "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "e4"; ":d"; ]
            |> algebraicChessStateMachine
        test <@
                outcomes
                |> Seq.last
                |> outcomeToPGN = "1. Nc3 Nc6 2. Nb1 Nb8 3. Nc3 Nc6 4. Nb1 Nb8 5. Nc3 Nc6 6. Nb1 Nb8 7. e4 {Draw offered}"
        @>

    [<Fact>]
    let ``Scholar's mate``() =
        [ "e4"; "e5"; "Bc4"; "Nc6"; "Qh5"; "Nf6"; "Qxf7" ]
        |> algebraicChessStateMachine
        |> Seq.last
        |> outcomeToPGN =! "1. e4 e5 2. Bc4 Nc6 3. Qh5 Nf6 4. Qxf7# 1-0"

    [<Fact>]
    let ``After a five-fold repetition, it's a draw``() =
        let outcomes =
            [ "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1";  ]
            |> algebraicChessStateMachine
        test <@
                outcomes
                |> Seq.last
                |> outcomeToPGN = "1. Nc3 Nc6 2. Nb1 Nb8 3. Nc3 Nc6 4. Nb1 Nb8 5. Nc3 Nc6 6. Nb1 Nb8 7. Nc3 Nc6 8. Nb1 Nb8 9. Nc3 Nc6 10. Nb1 Nb8 1/2-1/2 {FivefoldRepetition}"
        @>

    [<Fact>]
    let ``Scholar's mate FENs``() =
        test <@
                [ "e4"; "e5"; "Bc4"; "Nc6"; "Qh5"; "Nf6"; "Qxf7" ]
                |> algebraicChessStateMachine
                |> Seq.map (fun x -> x.displayInfo.gameState.toFEN)
                |> Seq.toList = [
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
                "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1";
                "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2";
                "rnbqkbnr/pppp1ppp/8/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2";
                "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR w KQkq - 2 3";
                "r1bqkbnr/pppp1ppp/2n5/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 3 3";
                "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4";
                "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"] 
        @>
        
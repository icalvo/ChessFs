namespace ChessFs.Tests


module ``transitions`` =
    open Xunit
    open Swensen.Unquote
    open Chess
    open Notation
    open StateMachine

    let findExecutableAction (input:string) availableActions =
         availableActions
         |> List.filter (fun { action = m } -> (playerActionToAlgebraic m).ToLowerInvariant() = input.ToLowerInvariant())
         |> List.tryHead
    let equality m (input:string) = (playerActionToAlgebraic m).ToLowerInvariant() = input.ToLowerInvariant()
    [<Fact>]
    let ``Fool's mate``() =
        test <@
                [ "f3"; "e5"; "g4"; "Qh4" ]
                |> algebraicChessStateMachine
                |> List.last
                |> outcomeToPGN = "1. f3 e5 2. g4 Qh4# 0-1"
        @>

    [<Fact>]
    let ``Draw offers``() =
        test <@
                [ "f3"; ":d"; ":d"; "e5"; ":d"; ":a" ]
                |> algebraicChessStateMachine
                |> List.last
                |> outcomeToPGN = "1. f3 e5 1/2-1/2 {Agreement}"
        @>

    [<Fact>]
    let ``Draw offers steps``() =
        test <@
                [ "f3"; ":d"; ":d"; "e5"; ":d"; ":a" ]
                |> algebraicChessStateMachine
                |> List.map (fun x -> sprintf "%s {%s}" (outcomeToPGN x) (x.displayInfo.gameState.statusToFEN)) = [
                " {w KQkq - 0 1}";
                "1. f3 {b KQkq - 0 1}";
                "1. f3 {Draw offered} {w KQkq - 0 1}";
                "1. f3 {Draw declined} {b KQkq - 0 1}";
                "1. f3 e5 {w KQkq e6 0 2}";
                "1. f3 e5 {Draw offered} {b KQkq e6 0 2}";
                "1. f3 e5 1/2-1/2 {Agreement} {b KQkq e6 0 2}"] 
        @>

    [<Fact>]
    let ``Scholar's mate``() =
        test <@
                [ "e4"; "e5"; "Bc4"; "Nc6"; "Qh5"; "Nf6"; "Qxf7" ]
                |> algebraicChessStateMachine
                |> List.last
                |> outcomeToPGN = "1. e4 e5 2. Bc4 Nc6 3. Qh5 Nf6 4. Qxf7# 1-0"
        @>

    [<Fact>]
    let ``Five-fold repetition``() =
        let outcomes =
            [ "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8";  ]
            |> algebraicChessStateMachine
        test <@
                outcomes
                |> List.last
                |> outcomeToPGN = "1. Nc3 Nc6 2. Nb1 Nb8 3. Nc3 Nc6 4. Nb1 Nb8 5. Nc3 Nc6 6. Nb1 Nb8 7. Nc3 Nc6 8. Nb1 Nb8 9. Nc3 Nc6 10. Nb1 1/2-1/2 {FivefoldRepetition}"
        @>

    [<Fact>]
    let ``Five-fold repetition 2``() =
        let outcomes =
            [ "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8";  ]
            |> algebraicChessStateMachine
        test <@
                outcomes
                |> List.map (fun x -> x.displayInfo.gameState.repeated) = [1;1]
        @>

    [<Fact>]
    let ``Scholar's mate FENs``() =
        test <@
                [ "e4"; "e5"; "Bc4"; "Nc6"; "Qh5"; "Nf6"; "Qxf7" ]
                |> algebraicChessStateMachine
                |> List.map (fun x -> x.displayInfo.gameState.toFEN) = [
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
                "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1";
                "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2";
                "rnbqkbnr/pppp1ppp/8/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2";
                "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR w KQkq - 2 3";
                "r1bqkbnr/pppp1ppp/2n5/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 3 3";
                "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4";
                "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"] 
        @>
        
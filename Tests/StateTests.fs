namespace ChessFs.Tests.ChessStateMachine

module ``transitions`` =
    open Xunit
    open Swensen.Unquote
    open Utils
    open Engine
    open Notation
    open ChessStateMachine

    let ResultDefault fn =
        function
        | Ok x -> fn x
        | Error x -> "Failure: " + x

    let finalState =
        algebraicStringChessStateMachine
        >> Seq.last
        >> Result.map PlayerActionOutcome.outcomeToResult
        >> Result.mapError (fun x -> "Failure: " + x)
        >> Result.toValue

    let finalPGN: string seq -> string =
        algebraicStringChessStateMachine
        >> Seq.last
        >> Result.map PlayerActionOutcome.toCommentedPGN
        >> Result.mapError (fun x -> "Failure: " + x)
        >> Result.toValue

    let outcomes =
        let statusToFEN = PlayerActionOutcome.state >> ChessState.statusToFEN

        algebraicStringChessStateMachine
        >> Seq.map (ResultDefault (fun x -> $"%s{PlayerActionOutcome.toCommentedPGN x} {{%s{statusToFEN x}}}"))
        >> Seq.toList

    let startsWith (p: string) (s: string) = s.StartsWith(p)

    [<Fact>]
    let ``Invalid move``() =
        test <@ ["e6"] |> finalState |> startsWith "Failure: Could not find action e6." @>

    [<Fact>]
    let Stalemate() =
        ["e3"; "a5";"Qh5";"Ra6";"Qxa5";"h5";"h4";"Rah6";"Qxc7";"f6";"Qxd7";"Kf7";"Qxb7";"Qd3"; "Qxb8";"Qh7"; "Qxc8"; "Kg6"; "Qe6"]
         |> finalState =! "1/2-1/2 {Stalemate}"

    [<Fact>]
    let ``Fool's mate``() =
        [ "f3"; "e5"; "g4"; "Qh4" ]
        |> finalPGN =! "1. f3 e5 2. g4 Qh4# 0-1"

    [<Fact>]
    let ``Draw by agreement``() =
        [ "f3"; "e5"; "d3:d"; ":a" ]
        |> finalPGN =! "1. f3 e5 2. d3 1/2-1/2 {Agreement}"

    [<Fact>]
    let ``Draw declined and accepted``() =
        [ "f3:d"; "e5"; "d3:d"; ":a" ]
        |> outcomes =! [
        "{GameStarted} {w KQkq - 0 1}";
        "1. f3 {Draw offered} {b KQkq - 0 1}";
        "1. f3 e5 {w KQkq e6 0 2}";
        "1. f3 e5 2. d3 {Draw offered} {b KQkq - 0 2}";
        "1. f3 e5 2. d3 1/2-1/2 {Agreement} {b KQkq - 0 2}"]

    [<Fact>]
    let ``After three-fold repetition, a draw offer is automatically accepted``() =
        [ "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8:d"; ]
        |> finalPGN =! "1. Nc3 Nc6 2. Nb1 Nb8 3. Nc3 Nc6 4. Nb1 Nb8 5. Nc3 Nc6 6. Nb1 Nb8 1/2-1/2 {ThreefoldRepetition}"

    [<Fact>]
    let ``After three-fold repetition in the past, a draw offer is NOT automatically accepted``() =
        [ "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "Nc3"; "Nc6"; "Nb1"; "Nb8"; "e4:d"; ]
        |> finalPGN =! "1. Nc3 Nc6 2. Nb1 Nb8 3. Nc3 Nc6 4. Nb1 Nb8 5. Nc3 Nc6 6. Nb1 Nb8 7. e4 {Draw offered}"

    [<Fact>]
    let ``Check bug``() =
        [ "e4"; "e5"; "Bc4"; "Nc6"; "Qh5" ]
        |> finalPGN <>! "1. e4 e5 2. Bc4 Nc6 3. Qh5+"
    
    [<Fact>]
    let ``Scholar's mate``() =
        [ "e4"; "e5"; "Bc4"; "Nc6"; "Qh5"; "Nf6"; "Qxf7" ]
        |> finalPGN =! "1. e4 e5 2. Bc4 Nc6 3. Qh5 Nf6 4. Qxf7# 1-0"

    [<Fact>]
    let ``After a five-fold repetition, it's a draw``() =
        Seq.init 100 (fun _ -> [ "Nc3"; "Nc6"; "Nb1"; "Nb8" ])
        |> Seq.concat
        |> finalPGN =! "1. Nc3 Nc6 2. Nb1 Nb8 3. Nc3 Nc6 4. Nb1 Nb8 5. Nc3 Nc6 6. Nb1 Nb8 7. Nc3 Nc6 8. Nb1 Nb8 9. Nc3 Nc6 10. Nb1 Nb8 1/2-1/2 {FivefoldRepetition}"

    [<Fact>]
    let ``After 50 moves, a draw offer is automatically accepted``() =
        let initial = [ "e4"; "b6" ] |> List.toSeq
        let queenMoves = [ "Qh5"; "Qa5"; "Qb5"; "Qc5"; "Qd5"; "Qe5"; "Qf5"; "Qg5" ]
        let otherMoves = [ "Ba6"; "Nc3"; "Bb7"; "Nb1"; "Bc8" ]

        let cycle = queenMoves |> Seq.collect (fun queenMove -> queenMove::otherMoves)
        let cycles =
            Seq.init 200 (fun _ -> cycle )
            |> Seq.concat
        let append s2 s1 = Seq.append s1 s2

        let moves =
            initial
            |> append cycles
            |> Seq.take 53
            |> Seq.toList

        let makeLastMoveDrawOffer = List.mapi (fun idx item -> if idx = (List.length moves) - 1 then item + ":d" else item)
        moves
        |> makeLastMoveDrawOffer
        |> finalPGN =! "1. e4 b6 2. Qh5 Ba6 3. Nc3 Bb7 4. Nb1 Bc8 5. Qa5 Ba6 6. Nc3 Bb7 7. Nb1 Bc8 8. Qb5 Ba6 9. Nc3 Bb7 10. Nb1 Bc8 11. Qc5 Ba6 12. Nc3 Bb7 13. Nb1 Bc8 14. Qd5 Ba6 15. Nc3 Bb7 16. Nb1 Bc8 17. Qe5 Ba6 18. Nc3 Bb7 19. Nb1 Bc8 20. Qf5 Ba6 21. Nc3 Bb7 22. Nb1 Bc8 23. Qg5 Ba6 24. Nc3 Bb7 25. Nb1 Bc8 26. Qh5 Ba6 27. Nc3 1/2-1/2 {FiftyMovements}"

    [<Fact>]
    let ``After 75 moves, it's a draw``() =
        let initial = [ "e4"; "b6" ] |> List.toSeq
        let queenMoves = [ "Qh5"; "Qa5"; "Qb5"; "Qc5"; "Qd5"; "Qe5"; "Qf5"; "Qg5" ]
        let blackKnightMoves = [ "Ba6"; "Nc3"; "Bb7"; "Nb1"; "Bc8" ]

        let cycle = queenMoves |> Seq.collect (fun queenMove -> queenMove::blackKnightMoves)
        let cycles = Seq.init 200 (fun _ -> cycle ) |> Seq.concat

        let expected = Seq.concat [initial ; cycles ] |> finalPGN
        
        Assert.Equal(expected, "1. e4 b6 2. Qh5 Ba6 3. Nc3 Bb7 4. Nb1 Bc8 5. Qa5 Ba6 6. Nc3 Bb7 7. Nb1 Bc8 8. Qb5 Ba6 9. Nc3 Bb7 10. Nb1 Bc8 11. Qc5 Ba6 12. Nc3 Bb7 13. Nb1 Bc8 14. Qd5 Ba6 15. Nc3 Bb7 16. Nb1 Bc8 17. Qe5 Ba6 18. Nc3 Bb7 19. Nb1 Bc8 20. Qf5 Ba6 21. Nc3 Bb7 22. Nb1 Bc8 23. Qg5 Ba6 24. Nc3 Bb7 25. Nb1 Bc8 26. Qh5 Ba6 27. Nc3 Bb7 28. Nb1 Bc8 29. Qa5 Ba6 30. Nc3 Bb7 31. Nb1 Bc8 32. Qb5 Ba6 33. Nc3 Bb7 34. Nb1 Bc8 35. Qc5 Ba6 36. Nc3 Bb7 37. Nb1 Bc8 38. Qd5 Ba6 39. Nc3 Bb7 1/2-1/2 {SeventyFiveMovements}")

    [<Fact>]
    let ``Scholar's mate FENs``() =
        let toFEN = PlayerActionOutcome.state >> ChessState.toFEN

        [ "e4"; "e5"; "Bc4"; "Nc6"; "Qh5"; "Nf6"; "Qxf7" ]
        |> algebraicStringChessStateMachine
        |> Seq.map (ResultDefault toFEN)
        |> Seq.toList =! [
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1";
        "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2";
        "rnbqkbnr/pppp1ppp/8/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2";
        "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR w KQkq - 2 3";
        "r1bqkbnr/pppp1ppp/2n5/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 3 3";
        "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4";
        "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"] 

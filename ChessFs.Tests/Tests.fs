namespace ChessFs.Tests


module ``be ascending tests`` =
    open Xunit
    open Swensen.Unquote
    open CoreTypes
    open Chess
    open Output

    [<Fact>]
    let ``Directions tests``() =
        test <@ [A2;C8;H3] |> List.map UpRight  = [Some B3; None; None] @>

    [<Fact>]
    let ``Reach generator``() =
        let simplify = List.map Seq.toList >> List.map List.sort >> List.sort
        let simplify2 = List.map (fun (r, a) -> r |> Seq.toList |> List.sort, a) >> List.sort

        test <@ pawnDoubleMoveReaches Up C2 |> simplify = [[C3; C4]] @>
        test <@ pawnSingleMoveReaches Up C3 |> simplify = [[C4]] @>
        test <@ pawnCaptureReaches    Up C2 |> simplify = [[B3];[D3]] @>
        test
            <@ bishopReaches C3 |> simplify =
            [[A1; B2]; [A5; B4]; [D2; E1];[D4; E5; F6; G7; H8]]@>
        test
            <@ rookReaches C3 |> simplify =
            [[A3; B3]; [C1; C2]; [C4; C5; C6; C7; C8];[D3; E3; F3; G3; H3]]@>
        test
            <@ pieceReaches WhitePawn C2 |> simplify2 =
            [([B3], CaptureType); ([C3; C4], MoveType); ([D3], CaptureType)]@>
        test <@
                pieceReaches BlackPawn E7 |> simplify2 = [
                    ([D6], CaptureType);
                    ([E5; E6], MoveType);
                    ([F6], CaptureType)]
        @>
        test <@
                pieceReaches BlackKnight B8 |> simplify2 = [
                    ([A6], MoveType);
                    ([A6], CaptureType);
                    ([C6], MoveType);
                    ([C6], CaptureType);
                    ([D7], MoveType);
                    ([D7], CaptureType);
                    ]
        @>
    let result3 = 
        List.map playerActionToAlgebraic
        >> List.sort


    let result2 = 
        List.map (fun moveInfo -> moveInfo.action)
        >> result3
        >> List.sort
        
    let result = function
        | PlayerMoved (displayInfo, availableActions) ->
            result2 availableActions
        | _ -> List.empty


    [<Fact>]
    let ``Promoting available actions``() =
        test <@

                makePlayerMoveResultWithCapabilities {
                    turn = White
                    pieces =
                        [
                            placedPiece White Pawn   C7
                        ]
                    whitePlayerCastleState = cannotCastle
                    blackPlayerCastleState = cannotCastle
                    enPassantPawn = None
                }
                |> result = [":d"; ":r"; "c8=B"; "c8=N"; "c8=Q"; "c8=R"] 
        @>

    [<Fact>]
    let ``Capture-promoting available actions``() =
        test <@
                makePlayerMoveResultWithCapabilities {
                    turn = White
                    pieces =
                        [
                            placedPiece White Pawn   C7
                            placedPiece Black Rook   B8
                        ]
                    whitePlayerCastleState = cannotCastle
                    blackPlayerCastleState = cannotCastle
                    enPassantPawn = None
                }
                |> result = [":d"; ":r"; "c8=B"; "c8=N"; "c8=Q"; "c8=R"; "cxb8=B"; "cxb8=N"; "cxb8=Q"; "cxb8=R"]
        @>
    
    let gameStateAfterE4 = initialGameState |> nextGameState (Move (WhitePawn, E2, E4))

    let gameStateWithCheck =
        initialGameState
        |> nextGameState (Move (WhitePawn, E2, E4))
        |> nextGameState (Move (BlackPawn, E7, E5))
        |> nextGameState (Move (WhitePawn, F2, F3))
        |> nextGameState (Move (Piece (Black, Queen), D8, H4))

    [<Fact>]
    let ``Black available actions``() =
        test <@
                makePlayerMoveResultWithCapabilities gameStateAfterE4
                |> result = [":d"; ":r"; "Na6"; "Nc6"; "Nf6"; "Nh6"; "a5"; "a6"; "b5"; "b6"; "c5"; "c6"; "d5"; "d6"; "e5"; "e6"; "f5";
 "f6"; "g5"; "g6"; "h5"; "h6"]
        @>

    [<Fact>]
    let ``Black available player actions``() =
        test <@
                playerActions gameStateAfterE4
                |> Seq.toList
                |> result3 = [":d"; ":r"; "Na6"; "Nc6"; "Nf6"; "Nh6"; "a5"; "a6"; "b5"; "b6"; "c5"; "c6"; "d5"; "d6"; "e5"; "e6"; "f5";
 "f6"; "g5"; "g6"; "h5"; "h6"]
        @>

    [<Fact>]
    let ``Black piece capabilities``() =
        test <@
                pieceCapabilities gameStateAfterE4 (Piece (Black, Pawn), E7)
                |> Seq.toList = [
                    Move (Piece (Black,Pawn), E7, E6);
                    Move (Piece (Black,Pawn), E7, E5)]
        @>


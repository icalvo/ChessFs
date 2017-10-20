namespace ChessFs.Tests


module ``be ascending tests`` =
    open Xunit
    open Swensen.Unquote
    open Chess
    open Output

    [<Fact>]
    let ``UpRight tests``() =
        test <@ [A2;C8] |> List.map UpRight  = [Some B3; None] @>

    [<Fact>]
    let ``Reach generator``() =
        let simplify = List.map Seq.toList >> List.map List.sort >> List.sort
        let simplify2 = List.map (fun (r, a) -> r |> Seq.toList |> List.sort, a) >> List.sort

        test <@ whitePawnMoveReaches C2 |> simplify = [[C3; C4]] @>
        test <@ whitePawnMoveReaches C3 |> simplify = [[C4]] @>
        test <@ whitePawnCaptureReaches C2 |> simplify = [[B3];[D3]] @>
        test
            <@ bishopReaches C3 |> simplify =
            [[A1; B2]; [A5; B4]; [D2; E1];[D4; E5; F6; G7; H8]]@>
        test
            <@ rookReaches C3 |> simplify =
            [[A3; B3]; [C1; C2]; [C4; C5; C6; C7; C8];[D3; E3; F3; G3; H3]]@>
        test
            <@ pieceReaches2 WhitePawn C2 |> simplify2 =
            [([B3], Capture); ([C3; C4], Move); ([D3], Capture)]@>

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
                |> result = ["c8=B"; "c8=N"; "c8=Q"; "c8=R"; "d"; "r"] 
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
                |> result = ["c8=B"; "c8=N"; "c8=Q"; "c8=R"; "cxb8=B"; "cxb8=N"; "cxb8=Q"; "cxb8=R"; "d"; "r"]
        @>
    
    let gameStateAfterE4 = {
                    turn = Black
                    pieces =
                        [
                            placedPiece White Rook   A1
                            placedPiece White Knight B1
                            placedPiece White Bishop C1
                            placedPiece White Queen  D1
                            placedPiece White King   E1
                            placedPiece White Bishop F1
                            placedPiece White Knight G1
                            placedPiece White Rook   H1
                            placedPiece White Pawn   A2
                            placedPiece White Pawn   B2
                            placedPiece White Pawn   C2
                            placedPiece White Pawn   D2
                            placedPiece White Pawn   E4
                            placedPiece White Pawn   F2
                            placedPiece White Pawn   G2
                            placedPiece White Pawn   H2
                            placedPiece Black Pawn   A7
                            placedPiece Black Pawn   B7
                            placedPiece Black Pawn   C7
                            placedPiece Black Pawn   D7
                            placedPiece Black Pawn   E7
                            placedPiece Black Pawn   F7
                            placedPiece Black Pawn   G7
                            placedPiece Black Pawn   H7
                            placedPiece Black Rook   A8
                            placedPiece Black Knight B8
                            placedPiece Black Bishop C8
                            placedPiece Black Queen  D8
                            placedPiece Black King   E8
                            placedPiece Black Bishop F8
                            placedPiece Black Knight G8
                            placedPiece Black Rook   H8
                        ]
                    whitePlayerCastleState = cannotCastle
                    blackPlayerCastleState = cannotCastle
                    enPassantPawn = None
                }
    [<Fact>]
    let ``Black available actions``() =
        test <@
                makePlayerMoveResultWithCapabilities gameStateAfterE4
                |> result = ["c8=B"; "c8=N"; "c8=Q"; "c8=R"; "cxb8=B"; "cxb8=N"; "cxb8=Q"; "cxb8=R"; "d"; "r"]
        @>

    [<Fact>]
    let ``Black available player actions``() =
        test <@
                playerActions gameStateAfterE4
                |> Seq.toList
                |> result3 = ["c8=B"; "c8=N"; "c8=Q"; "c8=R"; "cxb8=B"; "cxb8=N"; "cxb8=Q"; "cxb8=R"; "d"; "r"]
        @>
﻿namespace ChessFs.Tests


module ``be ascending tests`` =
    open Xunit
    open Swensen.Unquote
    open CoreTypes
    open Chess
    open Notation
    open StateMachine

    let WhitePawn = Piece (White, Pawn)
    let BlackPawn = Piece (Black, Pawn)
    let BlackKnight = Piece (Black, Knight)

    [<Fact>]
    let ``Directions tests``() =
        test <@ [A2;C8;H3] |> List.map Reach.UpRight  = [Some B3; None; None] @>

    [<Fact>]
    let ``Reach generator``() =
        let normalize = List.map Seq.toList >> List.map List.sort >> List.sort
        let normalize2 = List.map (fun (r, a) -> r |> Seq.toList |> List.sort, a) >> List.sort

        test <@ Reach.pawnDoubleMoveReaches Reach.Up C2 |> normalize = [[C3; C4]] @>
        test <@ Reach.pawnSingleMoveReaches Reach.Up C3 |> normalize = [[C4]] @>
        test <@ Reach.pawnCaptureReaches    Reach.Up C2 |> normalize = [[B3];[D3]] @>
        test
            <@ Reach.bishopReaches C3 |> normalize =
            [[A1; B2]; [A5; B4]; [D2; E1];[D4; E5; F6; G7; H8]]@>
        test
            <@ Reach.rookReaches C3 |> normalize =
            [[A3; B3]; [C1; C2]; [C4; C5; C6; C7; C8];[D3; E3; F3; G3; H3]]@>
        test
            <@ Reach.pieceReaches WhitePawn C2 |> normalize2 =
            [([B3], CaptureType); ([C3; C4], MoveType); ([D3], CaptureType)]@>
        test <@
                Reach.pieceReaches BlackPawn E7 |> normalize2 = [
                    ([D6], CaptureType);
                    ([E5; E6], MoveType);
                    ([F6], CaptureType)]
        @>
        test <@
                Reach.pieceReaches BlackKnight B8 |> normalize2 = [
                    ([A6], MoveType);
                    ([A6], CaptureType);
                    ([C6], MoveType);
                    ([C6], CaptureType);
                    ([D7], MoveType);
                    ([D7], CaptureType);
                    ]
        @>

    let result4 = 
        List.map plyToAlgebraic
        >> List.sort

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
                    whitePlayerCastleState = canCastle
                    blackPlayerCastleState = canCastle
                    pawnCapturableEnPassant = None
                    plies = []
                    pliesWithoutPawnOrCapture = 0
                    repeatableStates = []
                    numberOfMoves = 1
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
                    whitePlayerCastleState = canCastle
                    blackPlayerCastleState = canCastle
                    pawnCapturableEnPassant = None
                    plies = []
                    pliesWithoutPawnOrCapture = 0
                    repeatableStates = []
                    numberOfMoves = 1
                }
                |> result = [":d"; ":r"; "c8=B"; "c8=N"; "c8=Q"; "c8=R"; "cxb8=B"; "cxb8=N"; "cxb8=Q"; "cxb8=R"]
        @>

    [<Fact>]
    let ``Castling not possible when intermediate squares are in check``() =
        test <@
                makePlayerMoveResultWithCapabilities {
                    turn = White
                    pieces =
                        [
                            placedPiece White King   E1
                            placedPiece White Rook   H1
                            placedPiece Black Rook   G8
                        ]
                    whitePlayerCastleState = canCastle
                    blackPlayerCastleState = canCastle
                    pawnCapturableEnPassant = None
                    plies = []
                    pliesWithoutPawnOrCapture = 0
                    repeatableStates = []
                    numberOfMoves = 1
                }
                |> result = [":d"; ":r"; "Kd1"; "Kd2"; "Ke2"; "Kf1"; "Kf2"; "O-O-O"; "Rf1"; "Rg1";
                "Rh2"; "Rh3"; "Rh4"; "Rh5"; "Rh6"; "Rh7"; "Rh8"] 
        @>

    [<Fact>]
    let ``Castling possible when rook attacked``() =
        test <@
                makePlayerMoveResultWithCapabilities {
                    turn = White
                    pieces =
                        [
                            placedPiece White King   E1
                            placedPiece White Rook   H1
                            placedPiece Black Rook   H8
                        ]
                    whitePlayerCastleState = canCastle
                    blackPlayerCastleState = canCastle
                    pawnCapturableEnPassant = None
                    plies = []
                    pliesWithoutPawnOrCapture = 0
                    repeatableStates = []
                    numberOfMoves = 1
                }
                |> result = [":d"; ":r"; "Kd1"; "Kd2"; "Ke2"; "Kf1"; "Kf2"; "O-O"; "O-O-O"; "Rf1"; "Rg1";
                "Rh2"; "Rh3"; "Rh4"; "Rh5"; "Rh6"; "Rh7"; "Rxh8"] 
        @>

    [<Fact>]
    let ``King cannot move to check``() =
        test <@
                makePlayerMoveResultWithCapabilities {
                    turn = White
                    pieces =
                        [
                            placedPiece White King   E1
                            placedPiece Black Rook   F8
                            placedPiece Black King   H8
                        ]
                    whitePlayerCastleState = { canCastleKingside = false; canCastleQueenside = false }
                    blackPlayerCastleState = { canCastleKingside = false; canCastleQueenside = false }
                    pawnCapturableEnPassant = None
                    plies = []
                    pliesWithoutPawnOrCapture = 0
                    repeatableStates = []
                    numberOfMoves = 1
                }
                |> result = [":d"; ":r"; "Kd1"; "Kd2"; "Ke2" ]
        @>

    let findExecutableActionAux (input: string) availableActions =
            availableActions
            |> List.filter (fun { action = m } -> (playerActionToAlgebraic m).ToLowerInvariant() = input.ToLowerInvariant())
            |> List.tryHead

    let handleChessActionOutcome2 formerPlayerActionOutcome (input: string) =
        match formerPlayerActionOutcome with
        | Some (PlayerMoved (_, availableActions)) ->
            match findExecutableActionAux input availableActions with
            | Some x -> Some (x.execute())
            | None -> None
        | _ -> None

    let gameStateAfterE4 = initialGameState |> nextGameState (Move (WhitePawn, E2, E4))

    let gameStateAfter game move =
        makePlayerMoveResultWithCapabilities game

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
    let ``Black available player plies``() =
        test <@
                playerPlies gameStateAfterE4
                |> Seq.toList
                |> result4 = ["Na6"; "Nc6"; "Nf6"; "Nh6"; "a5"; "a6"; "b5"; "b6"; "c5"; "c6"; "d5"; "d6"; "e5"; "e6"; "f5";
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

    let lastFEN moves =
        moves
        |> algebraicChessStateMachine
        |> Seq.last
        |> (fun x -> x.displayInfo.gameState.toFEN)

    [<Fact>]
    let ``FEN tests``() =
        test <@ boardToFEN (getDisplayInfo initialGameState).board = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" @>
        test <@ boardToFEN (getDisplayInfo gameStateAfterE4).board = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR" @>
        test <@ [ ] |> lastFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" @>
        test <@ [ "e4" ] |> lastFEN = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" @>
        test <@ [ "e4"; "e5" ] |> lastFEN = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2" @>


namespace ChessFs.Tests.Chess


module ``Chess Tests`` =
    open Xunit
    open Swensen.Unquote
    open Utils
    open CoreTypes
    open Chess
    open Notation
    open ChessStateMachine

    [<Fact>]
    let ``Directions tests``() =
        [A2;C8;H3] |> List.map Reach.UpRight  =! [Some B3; None; None]

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

    let result = function
        | GameStarted (_, availableActions)
        | PlayerMoved (_, availableActions) ->
            List.map executableActionToAlgebraic availableActions |> List.sort
        | _ -> List.empty


    [<Fact>]
    let ``Promoting available actions``() =
        getOutcomeFromNewBoard {
            playerInTurn = White
            pieces =
                Map.ofList [
                    C7, WhitePawn
                ]
            whitePlayerCastlingRights = bothWaysCastlingRights
            blackPlayerCastlingRights = bothWaysCastlingRights
            pawnCapturableEnPassant = None
            plies = []
            pliesWithoutPawnOrCapture = 0
            repeatableStates = []
            numberOfMoves = 1
        }
        |> result =! [":d"; ":r"; "c8=B"; "c8=N"; "c8=Q"; "c8=R"]


    [<Fact>]
    let ``Capture-promoting available actions``() =
                getOutcomeFromNewBoard {
                    playerInTurn = White
                    pieces =
                        Map.ofList [
                            C7, WhitePawn
                            B8, BlackRook
                        ]
                    whitePlayerCastlingRights = bothWaysCastlingRights
                    blackPlayerCastlingRights = bothWaysCastlingRights
                    pawnCapturableEnPassant = None
                    plies = []
                    pliesWithoutPawnOrCapture = 0
                    repeatableStates = []
                    numberOfMoves = 1
                }
                |> result =! [":d"; ":r"; "c8=B"; "c8=N"; "c8=Q"; "c8=R"; "cxb8=B"; "cxb8=N"; "cxb8=Q"; "cxb8=R"]


    [<Fact>]
    let ``Castling not possible when intermediate squares are in check``() =
        test <@
                getOutcomeFromNewBoard {
                    playerInTurn = White
                    pieces =
                        Map.ofList [
                            E1, WhiteKing
                            A1, WhiteRook
                            H1, WhiteRook
                            G8, BlackRook
                        ]
                    whitePlayerCastlingRights = bothWaysCastlingRights
                    blackPlayerCastlingRights = bothWaysCastlingRights
                    pawnCapturableEnPassant = None
                    plies = []
                    pliesWithoutPawnOrCapture = 0
                    repeatableStates = []
                    numberOfMoves = 1
                }
                |> result = [":d"; ":r"; "Kd1"; "Kd2"; "Ke2"; "Kf1"; "Kf2"; "O-O-O"; "Ra2"; "Ra3"; "Ra4";
                "Ra5"; "Ra6"; "Ra7"; "Ra8"; "Rb1"; "Rc1"; "Rd1"; "Rf1"; "Rg1";
                "Rh2"; "Rh3"; "Rh4"; "Rh5"; "Rh6"; "Rh7"; "Rh8"] 
        @>

    [<Fact>]
    let ``Castling possible when rook attacked``() =
            getOutcomeFromNewBoard {
                playerInTurn = White
                pieces =
                    Map.ofList [
                        E1, WhiteKing
                        H1, WhiteRook
                        A1, WhiteRook
                        H8, BlackRook
                    ]
                whitePlayerCastlingRights = bothWaysCastlingRights
                blackPlayerCastlingRights = bothWaysCastlingRights
                pawnCapturableEnPassant = None
                plies = []
                pliesWithoutPawnOrCapture = 0
                repeatableStates = []
                numberOfMoves = 1
            }
            |> result =! [":d"; ":r"; "Kd1"; "Kd2"; "Ke2"; "Kf1"; "Kf2"; "O-O"; "O-O-O"; "Ra2"; "Ra3"; "Ra4";
            "Ra5"; "Ra6"; "Ra7"; "Ra8"; "Rb1"; "Rc1"; "Rd1"; "Rf1"; "Rg1";
            "Rh2"; "Rh3"; "Rh4"; "Rh5"; "Rh6"; "Rh7"; "Rxh8"] 

    [<Fact>]
    let ``King cannot move to check``() =
        test <@
                getOutcomeFromNewBoard {
                    playerInTurn = White
                    pieces =
                        Map.ofList [
                            E1, WhiteKing
                            F8, BlackRook
                            H8, BlackKing
                        ]
                    whitePlayerCastlingRights = { canCastleKingSide = false; canCastleQueenSide = false }
                    blackPlayerCastlingRights = { canCastleKingSide = false; canCastleQueenSide = false }
                    pawnCapturableEnPassant = None
                    plies = []
                    pliesWithoutPawnOrCapture = 0
                    repeatableStates = []
                    numberOfMoves = 1
                }
                |> result = [":d"; ":r"; "Kd1"; "Kd2"; "Ke2" ]
        @>

    let internal gameStateAfterE4 = initialGameState |> nextGameState (Move (WhitePawn, E2, E4)) []

    [<Fact>]
    let ``Black piece capabilities``() =
        test <@
                pieceCapabilities gameStateAfterE4 (Piece (Black, Pawn), E7)
                |> Seq.toList = [
                    Move (BlackPawn, E7, E6);
                    Move (BlackPawn, E7, E5)]
        @>

    let ResultDefault fn = Result.defaultWith fn (fun _ _ -> "Failure")

    let lastFEN moves =
        moves
        |> algebraicStringChessStateMachine
        |> Seq.last
        |> ResultDefault (PlayerActionOutcome.representation >> ChessStateRepresentation.toFEN)

    [<Fact>]
    let ``FEN tests``() =
        boardToFEN (representation initialGameState).board =! "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
        boardToFEN (representation gameStateAfterE4).board =! "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"
        [ ] |> lastFEN =! "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        [ "e4" ] |> lastFEN =! "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        [ "e4"; "e5" ] |> lastFEN =! "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"


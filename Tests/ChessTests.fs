namespace ChessFs.Tests.Chess

module ``Chess Tests`` =
    open Xunit
    open Swensen.Unquote
    open Utils
    open CoreTypes
    open Engine
    open Notation
    open ChessStateMachine

    [<Fact>]
    let ``Directions tests``() =
        [A2;C8;H3] |> List.map PieceReaches.UpRight  =! [Some B3; None; None]

    [<Fact>]
    let ``Reach generation``() =
        let normalize = List.map Seq.toList >> List.map List.sort >> List.sort
        let normalize2 = List.map (fun r -> r |> Seq.toList |> List.sort) >> List.sort

        test <@ PieceReaches.pawnDoubleMoveReaches PieceReaches.Up C2 |> normalize = [[C3; C4]] @>
        test <@ PieceReaches.pawnSingleMoveReaches PieceReaches.Up C3 |> normalize = [[C4]] @>
        test <@ PieceReaches.pawnCaptureReaches    PieceReaches.Up C2 |> normalize = [[B3];[D3]] @>
        test
            <@ PieceReaches.bishopReaches C3 |> normalize =
            [[A1; B2]; [A5; B4]; [D2; E1];[D4; E5; F6; G7; H8]]@>
        test
            <@ PieceReaches.rookReaches C3 |> normalize =
            [[A3; B3]; [C1; C2]; [C4; C5; C6; C7; C8];[D3; E3; F3; G3; H3]]@>
        test
            <@ PieceReaches.pieceReaches WhitePawn C2 |> normalize2 = [
                 [Move (WhitePawn, C2, C3);Move (WhitePawn, C2, C4)]
                 [Capture (WhitePawn, C2, B3)]
                 [Capture (WhitePawn, C2, D3)]
            ]
            @>

        test
            <@ PieceReaches.pieceReaches WhitePawn C7 |> normalize2 = [
                [MoveAndPromote (White, C7, C8, Knight)];
                [MoveAndPromote (White, C7, C8, Bishop)];
                [MoveAndPromote (White, C7, C8, Rook)];
                [MoveAndPromote (White, C7, C8, Queen)];
                [CaptureAndPromote (White, C7, B8, Knight)];
                [CaptureAndPromote (White, C7, B8, Bishop)];
                [CaptureAndPromote (White, C7, B8, Rook)];
                [CaptureAndPromote (White, C7, B8, Queen)];
                [CaptureAndPromote (White, C7, D8, Knight)];
                [CaptureAndPromote (White, C7, D8, Bishop)];
                [CaptureAndPromote (White, C7, D8, Rook)];
                [CaptureAndPromote (White, C7, D8, Queen)]]
            @>
        test
            <@ PieceReaches.pieceReaches BlackPawn E2 |> normalize2 = [
                [MoveAndPromote    (Black, E2, E1, Knight)];
                [MoveAndPromote    (Black, E2, E1, Bishop)];
                [MoveAndPromote    (Black, E2, E1, Rook)];
                [MoveAndPromote    (Black, E2, E1, Queen)];
                [CaptureAndPromote (Black, E2, D1, Knight)];
                [CaptureAndPromote (Black, E2, D1, Bishop)];
                [CaptureAndPromote (Black, E2, D1, Rook)];
                [CaptureAndPromote (Black, E2, D1, Queen)];
                [CaptureAndPromote (Black, E2, F1, Knight)];
                [CaptureAndPromote (Black, E2, F1, Bishop)];
                [CaptureAndPromote (Black, E2, F1, Rook)];
                [CaptureAndPromote (Black, E2, F1, Queen)]]
        @>
        test <@
                PieceReaches.pieceReaches BlackKnight B8 |> normalize2 = [
                     [Move    (BlackKnight, B8, A6)]
                     [Move    (BlackKnight, B8, C6)]
                     [Move    (BlackKnight, B8, D7)]
                     [Capture (BlackKnight, B8, A6)]
                     [Capture (BlackKnight, B8, C6)]
                     [Capture (BlackKnight, B8, D7)]
                    ]
        @>

    let availableActions = function
        | Ok x ->
            match x with
            | GameStarted (_, availableActions)
            | DrawOffered (_, availableActions)
            | PlayerMoved (_, availableActions) ->
                List.map executableActionToAlgebraic availableActions
            | _ -> List.empty
        | Error msg -> ["Invalid board: " + msg]
 
    [<Fact>]
    let ``Promoting available actions``() =
        {
            playerInTurn = Player White
            pieces =
                Map.ofList [
                    C7, WhitePawn
                    E1, WhiteKing
                    H8, BlackKing
                ]
            whitePlayerCastlingRights = CastlingRights.none
            blackPlayerCastlingRights = CastlingRights.none
            pawnCapturableEnPassant = None
            pliesWithoutPawnOrCapture = 0
            numberOfMoves = 1
        }
        |> ChessState.fromRaw
        |> Result.bind setupStandardChessPosition
        |> availableActions
        |> Assert.supersetOf ["c8=B"; "c8=N"; "c8=Q"; "c8=R"]

    [<Fact>]
    let ``Capture-promoting available actions``() =
        {
            playerInTurn = Player White
            pieces =
                Map.ofList [
                    C7, WhitePawn
                    B8, BlackRook
                    E1, WhiteKing
                    H8, BlackKing
                ]
            whitePlayerCastlingRights = CastlingRights.none
            blackPlayerCastlingRights = CastlingRights.none
            pawnCapturableEnPassant = None
            pliesWithoutPawnOrCapture = 0
            numberOfMoves = 1
        }
        |> ChessState.fromRaw
        |> Result.bind setupStandardChessPosition
        |> availableActions
        |> Assert.supersetOf [
            "c8=B"; "c8=N"; "c8=Q"; "c8=R";
            "cxb8=B"; "cxb8=N"; "cxb8=Q"; "cxb8=R"]

    [<Fact>]
    let ``Castling not possible when intermediate squares are in check``() =
        {
            playerInTurn = Player White
            pieces =
                Map.ofList [
                    E1, WhiteKing
                    A1, WhiteRook
                    H1, WhiteRook
                    G8, BlackRook
                    E4, BlackKing
                ]
            whitePlayerCastlingRights = CastlingRights.justKingside
            blackPlayerCastlingRights = CastlingRights.none
            pawnCapturableEnPassant = None
            pliesWithoutPawnOrCapture = 0
            numberOfMoves = 1
        }
        |> ChessState.fromRaw
        |> Result.bind setupStandardChessPosition
        |> availableActions
        |> Assert.doesNotContain "O-O"

    [<Fact>]
    let ``Castling possible when rook attacked``() =
        {
            playerInTurn = Player White
            pieces =
                Map.ofList [
                    E1, WhiteKing
                    H1, WhiteRook
                    A1, WhiteRook
                    H8, BlackRook
                    E3, BlackKing
                ]
            whitePlayerCastlingRights = CastlingRights.justKingside
            blackPlayerCastlingRights = CastlingRights.none
            pawnCapturableEnPassant = None
            pliesWithoutPawnOrCapture = 0
            numberOfMoves = 1
        }
        |> ChessState.fromRaw
        |> Result.bind setupStandardChessPosition
        |> availableActions
        |> Assert.contains "O-O"

    [<Fact>]
    let ``King cannot move to check``() =
        {
            playerInTurn = Player White
            pieces =
                Map.ofList [
                    E1, WhiteKing
                    F8, BlackRook
                    H8, BlackKing
                ]
            whitePlayerCastlingRights = CastlingRights.none
            blackPlayerCastlingRights = CastlingRights.none
            pawnCapturableEnPassant = None
            pliesWithoutPawnOrCapture = 0
            numberOfMoves = 1
        }
        |> ChessState.fromRaw
        |> Result.bind setupStandardChessPosition
        |> availableActions
        |> Assert.doesNotContain "Kf1"

    open ChessState

    let throwOnError = function
        | Ok r -> r
        | Error x -> failwith $"{x}"

    let internal boardAfter =
        algebraicStringChessFailingStateMachine
        >> Seq.last
        >> throwOnError
        >> PlayerActionOutcome.state
        >> board

    let lastRepToString fn =
        algebraicStringChessStateMachine
        >> Seq.last
        >> Result.toValue2 (PlayerActionOutcome.state >> fn) "Failure"

    [<Fact>]
    let ``FEN tests``() =
        [ ] |> boardAfter |> boardToFEN =!
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
        [ "e4" ] |> boardAfter |> boardToFEN =!
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"
        [ ] |> lastRepToString toFEN =! "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        [ "e4" ] |> lastRepToString toFEN =! "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        [ "e4"; "e5" ] |> lastRepToString toFEN =! "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"


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
        | Ok x ->
            match x with
            | GameStarted (_, availableActions)
            | PlayerMoved (_, availableActions, _) ->
                List.map executableActionToAlgebraic availableActions |> Set.ofList
            | _ -> Set.empty
        | Error msg -> Set.ofList ["Invalid board: " + msg]


    [<Fact>]
    let ``Promoting available actions``() =
        let expectedActions = Set.ofList ["c8=B"; "c8=N"; "c8=Q"; "c8=R"]

        setupStandardChessPosition {
            playerInTurn = White
            pieces =
                Map.ofList [
                    C7, WhitePawn
                    E1, WhiteKing
                    H8, BlackKing
                ]
            whitePlayerCastlingRights = noCastlingRights
            blackPlayerCastlingRights = noCastlingRights
            pawnCapturableEnPassant = None
            plies = []
            pliesWithoutPawnOrCapture = 0
            repeatableStates = []
            numberOfMoves = 1
        } false
        |> result
        |> Set.intersect expectedActions
        =! expectedActions

    module Set =
        let containsAll l set = List.forall (fun x -> Set.contains x set) l

    [<Fact>]
    let ``Capture-promoting available actions``() =
        test <@
            setupStandardChessPosition {
                playerInTurn = White
                pieces =
                    Map.ofList [
                        C7, WhitePawn
                        B8, BlackRook
                        E1, WhiteKing
                        H8, BlackKing
                    ]
                whitePlayerCastlingRights = noCastlingRights
                blackPlayerCastlingRights = noCastlingRights
                pawnCapturableEnPassant = None
                plies = []
                pliesWithoutPawnOrCapture = 0
                repeatableStates = []
                numberOfMoves = 1
            } false
            |> result |> Set.containsAll ["c8=B"; "c8=N"; "c8=Q"; "c8=R";
            "cxb8=B"; "cxb8=N"; "cxb8=Q"; "cxb8=R"] = true @>


    [<Fact>]
    let ``Castling not possible when intermediate squares are in check``() =
        setupStandardChessPosition {
            playerInTurn = White
            pieces =
                Map.ofList [
                    E1, WhiteKing
                    A1, WhiteRook
                    H1, WhiteRook
                    G8, BlackRook
                    E4, BlackKing
                ]
            whitePlayerCastlingRights = { canCastleKingSide = true; canCastleQueenSide = false }
            blackPlayerCastlingRights = noCastlingRights
            pawnCapturableEnPassant = None
            plies = []
            pliesWithoutPawnOrCapture = 0
            repeatableStates = []
            numberOfMoves = 1
        } false
        |> result |> Set.contains "O-O" =! false

    [<Fact>]
    let ``Castling possible when rook attacked``() =
            setupStandardChessPosition {
                playerInTurn = White
                pieces =
                    Map.ofList [
                        E1, WhiteKing
                        H1, WhiteRook
                        A1, WhiteRook
                        H8, BlackRook
                        E3, BlackKing
                    ]
                whitePlayerCastlingRights = { canCastleKingSide = true; canCastleQueenSide = false }
                blackPlayerCastlingRights = noCastlingRights
                pawnCapturableEnPassant = None
                plies = []
                pliesWithoutPawnOrCapture = 0
                repeatableStates = []
                numberOfMoves = 1
            } false
            |> result |> Set.contains "O-O" =! true

    [<Fact>]
    let ``King cannot move to check``() =
        setupStandardChessPosition {
            playerInTurn = White
            pieces =
                Map.ofList [
                    E1, WhiteKing
                    F8, BlackRook
                    H8, BlackKing
                ]
            whitePlayerCastlingRights = noCastlingRights
            blackPlayerCastlingRights = noCastlingRights
            pawnCapturableEnPassant = None
            plies = []
            pliesWithoutPawnOrCapture = 0
            repeatableStates = []
            numberOfMoves = 1
        } false
        |> result =! Set.ofList [":r"; "Kd1"; "Kd1:d"; "Kd2"; "Kd2:d"; "Ke2"; "Ke2:d" ]

    let internal gameStateAfterE4 = algebraicStringChessIgnoringStateMachine ["e4"] |> Seq.last |> PlayerActionOutcome.representation

    let ResultDefault fn =
        function
        | Ok x -> fn x
        | Error x -> "Failure"

    let lastFEN moves =
        moves
        |> algebraicStringChessStateMachine
        |> Seq.last
        |> ResultDefault (PlayerActionOutcome.representation >> ChessStateRepresentation.toFEN)

    [<Fact>]
    let ``FEN tests``() =
        boardToFEN (representation initialGameState).board =! "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
        boardToFEN gameStateAfterE4.board =! "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"
        [ ] |> lastFEN =! "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        [ "e4" ] |> lastFEN =! "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        [ "e4"; "e5" ] |> lastFEN =! "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"


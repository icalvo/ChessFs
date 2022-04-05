namespace ChessFs.Tests.Chess

open Xunit
open Swensen.Unquote
open ChessFs.Chess
open ChessFs.Chess.Engine
open ChessFs.Tests

module ``Basics Tests`` =
    open Coordinate
    open Piece
    
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

module ``Promotion Tests`` =
    open ChessFs.Common
    open Coordinate
    open Piece
    open Setup
    open Notation
    open Helpers

    [<Fact>]
    let ``Validation: more than one white king`` () =
        {
            playerInTurn = Player White
            pieces =
                Map.ofList [
                    C7, WhiteKing
                    E1, WhiteKing
                    H8, BlackKing
                ]
            castlingRights = {  white = CastlingRights.none; black = CastlingRights.none }
            pawnCapturableEnPassant = None
            pliesWithoutPawnOrCapture = 0
            numberOfMoves = 1
        }
        |> ChessState.fromRaw
        |> (fun x -> match x with | Ok _ -> Assert.True false | Error x -> Assert.Equal(x, MoreThanOneKing White))

    [<Fact>]
    let ``Validation: more than one black king`` () =
        {
            playerInTurn = Player Black
            pieces =
                Map.ofList [
                    C7, BlackKing
                    E1, BlackKing
                    H1, WhiteKing
                ]
            castlingRights = {  white = CastlingRights.none; black = CastlingRights.none }
            pawnCapturableEnPassant = None
            pliesWithoutPawnOrCapture = 0
            numberOfMoves = 1
        }
        |> ChessState.fromRaw
        |> (fun x -> match x with | Ok _ -> Assert.True false | Error x -> Assert.Equal(x, MoreThanOneKing Black))

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
            castlingRights = {  white = CastlingRights.none; black = CastlingRights.none }
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
            castlingRights = {  white = CastlingRights.none; black = CastlingRights.none }
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

module ``Castling Tests`` =
    open Coordinate
    open Piece
    open Setup
    open Notation
    open Helpers

    [<Fact>]
    let ``Castling not possible when final square is in check``() =
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
            castlingRights = {  white = CastlingRights.justKingside; black = CastlingRights.none }
            pawnCapturableEnPassant = None
            pliesWithoutPawnOrCapture = 0
            numberOfMoves = 1
        }
        |> ChessState.fromRaw
        |> Result.bind setupStandardChessPosition
        |> availableActions
        |> Assert.doesNotContain "O-O"

    [<Fact>]
    let ``Castling not possible when intermediate squares are in check``() =
        {
            playerInTurn = Player White
            pieces =
                Map.ofList [
                    E1, WhiteKing
                    A1, WhiteRook
                    H1, WhiteRook
                    F8, BlackRook
                    E4, BlackKing
                ]
            castlingRights = {  white = CastlingRights.justKingside; black = CastlingRights.none }
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
            castlingRights = {  white = CastlingRights.justKingside; black = CastlingRights.none }
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
            castlingRights = {  white = CastlingRights.none; black = CastlingRights.none }
            pawnCapturableEnPassant = None
            pliesWithoutPawnOrCapture = 0
            numberOfMoves = 1
        }
        |> ChessState.fromRaw
        |> Result.bind setupStandardChessPosition
        |> availableActions
        |> Assert.doesNotContain "Kf1"

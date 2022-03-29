﻿namespace ChessFs.Tests.Notation


module ``Notation Tests`` =
    open Xunit
    open Swensen.Unquote
    open CoreTypes
    open Engine
    open Notation

    [<Fact>]
    let ``Shape tests``() =
        Pawn.toString =! "P"
        Knight.toString =! "N"

    [<Fact>]
    let ``Piece tests``() =
        (Piece (White, Queen)).toFEN =! "Q"
        (Piece (Black, Queen)).toFEN =! "q"

    [<Fact>]
    let ``Coordinate tests``() =
        fileToAlgebraic B4 =! "b"
        rankToAlgebraic B4 =! "4"
        coordToAlgebraic B4 =! "b4"

    [<Fact>]
    let ``Square tests``() =
        squareToString (EmptySquare B6) =! "  "
        squareToString (PieceSquare (WhiteKnight, G1)) =! "wN"

    let private rookMove (source, target) = Move (WhiteRook, source, target)
    let private knightMove (source, target) = Move (WhiteKnight, source, target)
    let private pawnCapture (source, target) = Capture (WhitePawn, source, target)
    let private pawnPromotionCapture (source, target) = CaptureAndPromote (White, source, target, Queen)

    [<Fact>]
    let ``plyToAlgebraic move check non-restOfPlies``() =
        PlyOutput.toAlgebraic (CheckPly (rookMove(A6, E6),
            [], NoDrawOffer)) =! "Re6+"
    [<Fact>]
    let ``plyToAlgebraic capture pawn non-restOfPlies``() =
        PlyOutput.toAlgebraic (RegularPly (pawnCapture(E4, F5),
            [], NoDrawOffer)) =! "exf5"
    [<Fact>]
    let ``plyToAlgebraic capture pawn restOfPlies``() =
        PlyOutput.toAlgebraic (RegularPly (pawnCapture(E4, F5),
            [pawnCapture(G4, F5)], NoDrawOffer)) =! "exf5"
    [<Fact>]
    let ``plyToAlgebraic capture promote restOfPlies``() =
        PlyOutput.toAlgebraic (RegularPly (pawnPromotionCapture(E7, F8),
            [pawnCapture(G7, F8)], NoDrawOffer)) =! "exf8=Q"
    [<Fact>]
    let ``plyToAlgebraic castle kingside``() =
        PlyOutput.toAlgebraic (RegularPly (CastleKingSide White, [], NoDrawOffer)) =! "O-O"
    [<Fact>]
    let ``plyToAlgebraic castle queenside``() =
        PlyOutput.toAlgebraic (RegularPly (CastleQueenSide Black, [], NoDrawOffer)) =! "O-O-O"
    [<Fact>]
    let ``plyToAlgebraic move non-restOfPlies``() =
        PlyOutput.toAlgebraic (RegularPly (rookMove(A6, E6),
            [], NoDrawOffer)) =! "Re6"
    [<Fact>]
    let ``plyToAlgebraic move other rook at the same rank``() =
        PlyOutput.toAlgebraic (RegularPly (rookMove(A6, E6),
            [rookMove(H6, E6)], NoDrawOffer)) =! "Rae6"
    [<Fact>]
    let ``plyToAlgebraic move other rook at the same file``() =
        PlyOutput.toAlgebraic (RegularPly (rookMove(E4, E6),
            [rookMove(E8, E6)], NoDrawOffer)) =! "R4e6"
    let ``plyToAlgebraic move other rook at different rank/file``() =
        PlyOutput.toAlgebraic (RegularPly (rookMove(A6, E6),
            [rookMove(E8, E6)], NoDrawOffer)) =! "Rae6"
    [<Fact>]
    let ``plyToAlgebraic move other knight at same file, another at same rank``() =
        PlyOutput.toAlgebraic (RegularPly (knightMove(C3, E4),
            [knightMove(C6, E4); knightMove(G3, E4)], NoDrawOffer)) =! "Nc3e4"

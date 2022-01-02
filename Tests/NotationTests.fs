namespace ChessFs.Tests.Notation


module ``Notation Tests`` =
    open Xunit
    open Swensen.Unquote
    open CoreTypes
    open Chess
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
        plyToAlgebraic (CheckPly (rookMove(A6, E6),
            [], false)) =! "Re6+"
    [<Fact>]
    let ``plyToAlgebraic capture pawn non-restOfPlies``() =
        plyToAlgebraic (RegularPly (pawnCapture(E4, F5),
            [], false)) =! "exf5"
    [<Fact>]
    let ``plyToAlgebraic capture pawn restOfPlies``() =
        plyToAlgebraic (RegularPly (pawnCapture(E4, F5),
            [pawnCapture(G4, F5)], false)) =! "exf5"
    [<Fact>]
    let ``plyToAlgebraic capture promote restOfPlies``() =
        plyToAlgebraic (RegularPly (pawnPromotionCapture(E7, F8),
            [pawnCapture(G7, F8)], false)) =! "exf8=Q"
    [<Fact>]
    let ``plyToAlgebraic castle kingside``() =
        plyToAlgebraic (RegularPly (CastleKingSide White, [], false)) =! "O-O"
    [<Fact>]
    let ``plyToAlgebraic castle queenside``() =
        plyToAlgebraic (RegularPly (CastleQueenSide Black, [], false)) =! "O-O-O"
    [<Fact>]
    let ``plyToAlgebraic move non-restOfPlies``() =
        plyToAlgebraic (RegularPly (rookMove(A6, E6),
            [], false)) =! "Re6"
    [<Fact>]
    let ``plyToAlgebraic move other rook at the same rank``() =
        plyToAlgebraic (RegularPly (rookMove(A6, E6),
            [rookMove(H6, E6)], false)) =! "Rae6"
    [<Fact>]
    let ``plyToAlgebraic move other rook at the same file``() =
        plyToAlgebraic (RegularPly (rookMove(E4, E6),
            [rookMove(E8, E6)], false)) =! "R4e6"
    let ``plyToAlgebraic move other rook at different rank/file``() =
        plyToAlgebraic (RegularPly (rookMove(A6, E6),
            [rookMove(E8, E6)], false)) =! "Rae6"
    [<Fact>]
    let ``plyToAlgebraic move other knight at same file, another at same rank``() =
        plyToAlgebraic (RegularPly (knightMove(C3, E4),
            [knightMove(C6, E4); knightMove(G3, E4)], false)) =! "Nc3e4"


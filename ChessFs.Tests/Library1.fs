namespace ChessFs.Tests


module ``be ascending tests`` =
    open Xunit
    open Swensen.Unquote
    open Chess

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

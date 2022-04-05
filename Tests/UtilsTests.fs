namespace ChessFs.Tests.Utils

open Xunit
open Swensen.Unquote
open ChessFs.Common

module ``Option`` =
    [<Fact>]
    let ``defaultTo``() =
        None |> Option.defaultValue 34 =! 34
        Some 127 |> Option.defaultValue 34 =! 127

    [<Fact>]
    let ``mapList``() =
        let listfn = function
        | 2 -> [ "a"; "b" ]
        | _ -> [ "other" ]

        None |> Option.mapList listfn =! []
        Some 2 |> Option.mapList listfn =! [ "a"; "b" ]
        Some 5 |> Option.mapList listfn =! [ "other" ]

module ``Seq`` =
    [<Fact>]
    let ``filterNones``() =
        Seq.filterNones [Some "a"; None; Some "b"] |> Seq.toList =! [ "a"; "b" ]
        Seq.filterNones [None; None ] |> Seq.toList =! []
        Seq.filterNones [] |> Seq.toList =! []

    [<Fact>]
    let ``takeWhileIncludingLast``() =
        [1; 3; 5; 6; 8; 9] |> Seq.takeWhileIncludingLast (fun x -> x <= 5) |> Seq.toList =! [ 1; 3; 5; 6 ]

    [<Fact>]
    let ``unfoldSimple``() =
        let listfn = function
        | 3 -> Some 16
        | 16 -> Some 19
        | _ -> None
        3 |> Seq.unfoldSimple listfn |> Seq.toList =! [ 16; 19 ]

    [<Fact>]
    let ``batch``() =
        [] |> Seq.batch 3 =! [[]]
        [1; 3; 5; 6; 8; 9 ] |> Seq.batch 2 =! [ [ 1; 3 ]; [5 ; 6 ]; [ 8; 9 ] ]
        [1; 3; 5; 6; 8; 9; 12 ] |> Seq.batch 3 =! [ [ 1; 3; 5 ] ; [6; 8; 9] ; [ 12 ] ]

module ``List`` =
    [<Fact>]
    let ``filterNones``() =
        List.filterNones [Some "a"; None; Some "b"] =! [ "a"; "b" ]
        List.filterNones [None; None ] =! []
        List.filterNones [] =! []

    [<Fact>]
    let ``apply``() =
        let funList = [
            fun x -> x + x;
            fun x -> x + 5;
            fun x -> x - 1
        ]

        funList |> List.apply 2 =! [ 4; 7; 1 ]

module ``Result`` =
    [<Fact>]
    let ``defaultWith``() =
        let fn n =
            match n with
            | 455 -> Ok "nice455"
            | _ -> Error ($"bad%i{n}", [ "not 455" ])

        Ok 455 |> Result.bind fn =! Ok "nice455"
        Ok 566 |> Result.bind fn =!  Error ("bad566", [ "not 455" ])
        Error ("314", [ "erro1"; "error2" ]) |> Result.bind fn =! Error ("314", [ "erro1"; "error2" ])

module ``Operators`` =
    open Option

    [<Fact>]
    let ``>?>``() =
        let fn1 = function
        | 321 -> Some 327
        | 455 -> Some 457
        | _ -> None

        let fn2 = function
        | 327 -> Some 508
        | 455 -> None
        | _ -> None

        let fn = fn1 >?> fn2

        fn 13 =! None
        fn 321 =! Some 508
        fn 455 =! None


    [<Fact>]
    let ``*``() =
        let fn1 = function
        | 321 -> Some 327
        | 327 -> Some 508
        | 455 -> Some 457
        | _ -> None

        let fn = fn1 * 2
        fn 13 =! None
        fn 321 =! Some 508
        fn 455 =! None


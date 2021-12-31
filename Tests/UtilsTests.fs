namespace ChessFs.Tests.Utils


module ``Option`` =
    open Xunit
    open Swensen.Unquote
    open Utils

    [<Fact>]
    let ``defaultTo``() =
        None |> Option.defaultTo 34 =! 34
        Some 127 |> Option.defaultTo 34 =! 127

    [<Fact>]
    let ``mapList``() =
        let listfn = function
        | 2 -> [ "a"; "b" ]
        | _ -> [ "other" ]

        None |> Option.mapList listfn =! []
        Some 2 |> Option.mapList listfn =! [ "a"; "b" ]
        Some 5 |> Option.mapList listfn =! [ "other" ]

module ``Seq`` =
    open Xunit
    open Swensen.Unquote
    open Utils

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
    open Xunit
    open Swensen.Unquote
    open Utils

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
    open Xunit
    open Swensen.Unquote
    open Utils

    [<Fact>]
    let ``map``() =
        Success 456 |> Result.map (fun x -> x + 2) =! Success 458
        Failure (413, ["error1"; "error2"]) |> Result.map (fun x -> x + 2) =! Failure (415, ["error1"; "error2"])

    [<Fact>]
    let ``retn``() =
        test <@ Result.retn 456 = Success 456 @>

    [<Fact>]
    let ``apply``() =
        let fn = fun x -> x + 2
        Success 455 |> Result.apply (Success fn) =! Success 457
        Success 455 |> Result.apply (Failure (fn, ["errorfn"])) =! Failure (457, ["errorfn"])
        Failure (413, ["errorres"]) |> Result.apply (Success fn) =! Failure (415, ["errorres"])
        Failure (413, ["errorres"]) |> Result.apply (Failure (fn, ["errorfn"])) =! Failure (415, ["errorfn"; "errorres"])

    [<Fact>]
    let ``bind``() =
        let fn n =
            match n with
            | 455 -> Success "nice455"
            | _ -> Failure ($"bad%i{n}", [ "not 455" ])

        Success 455 |> Result.bind fn =! Success "nice455"
        Success 566 |> Result.bind fn =!  Failure ("bad566", [ "not 455" ])
        Failure (314, [ "erro1"; "error2" ]) |> Result.bind fn =! Failure ("bad314", [ "erro1"; "error2"; "not 455" ])

module ``Operators`` =
    open Xunit
    open Swensen.Unquote
    open Utils

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


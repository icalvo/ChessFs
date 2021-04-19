namespace ChessFs.Tests


module ``Option`` =
    open Xunit
    open Swensen.Unquote
    open Utils

    [<Fact>]
    let ``defaultTo``() =
        test <@ None |> Option.defaultTo 34 = 34 @>
        test <@ Some 127 |> Option.defaultTo 34 = 127 @>

    [<Fact>]
    let ``mapList``() =
        let listfn = function
        | 2 -> [ "a"; "b" ]
        | _ -> [ "other" ]

        test <@ None |> Option.mapList listfn = [] @>
        test <@ Some 2 |> Option.mapList listfn = [ "a"; "b" ] @>
        test <@ Some 5 |> Option.mapList listfn = [ "other" ] @>

module ``Seq`` =
    open Xunit
    open Swensen.Unquote
    open Utils

    [<Fact>]
    let ``filterNones``() =
        test <@ Seq.filterNones [Some "a"; None; Some "b"] |> Seq.toList = [ "a"; "b" ] @>
        test <@ Seq.filterNones [None; None ] |> Seq.toList = [] @>
        test <@ Seq.filterNones [] |> Seq.toList = [] @>

    [<Fact>]
    let ``takeWhilePlusOne``() =
        test <@ [1; 3; 5; 6; 8; 9] |> Seq.takeWhileIncludingLast (fun x -> x <= 5) |> Seq.toList = [ 1; 3; 5; 6 ] @>

    [<Fact>]
    let ``unfoldSimple``() =
        let listfn = function
        | 3 -> Some 16
        | 16 -> Some 19
        | _ -> None
        test <@ 3 |> Seq.unfoldSimple listfn |> Seq.toList = [ 16; 19 ] @>

    [<Fact>]
    let ``batch``() =
        test <@ [1; 3; 5; 6; 8; 9; 12 ] |> Seq.batch 3 = [ [ 1; 3; 5 ] ; [6; 8; 9] ; [ 12 ] ] @>

module ``List`` =
    open Xunit
    open Swensen.Unquote
    open Utils

    [<Fact>]
    let ``filterNones``() =
        test <@ List.filterNones [Some "a"; None; Some "b"] = [ "a"; "b" ] @>
        test <@ List.filterNones [None; None ] = [] @>
        test <@ List.filterNones [] = [] @>

    [<Fact>]
    let ``apply``() =
        let funList = [
            fun x -> x + x;
            fun x -> x + 5;
            fun x -> x - 1
        ]

        test <@ funList |> List.apply 2 = [ 4; 7; 1 ] @>

module ``Result`` =
    open Xunit
    open Swensen.Unquote
    open Utils

    [<Fact>]
    let ``map``() =
        test <@ Success 456 |> Result.map (fun x -> x + 2) = Success 458 @>
        test <@ Failure ["error1"; "error2"] |> Result.map (fun x -> x + 2) = Failure ["error1"; "error2"] @>

    [<Fact>]
    let ``retn``() =
        test <@ Result.retn 456 = Success 456 @>

    [<Fact>]
    let ``apply``() =
        let fn = fun x -> x + 2
        test <@ Success 455 |> Result.apply (Success fn) = Success 457 @>
        test <@ Success 455 |> Result.apply (Failure ["errorfn"]) = Failure ["errorfn"] @>
        test <@ Failure ["errorres"] |> Result.apply (Success fn) = Failure ["errorres"] @>
        test <@ Failure ["errorres"] |> Result.apply (Failure ["errorfn"]) = Failure ["errorfn"; "errorres"] @>

    [<Fact>]
    let ``bind``() =
        let fn = function
        | 455 -> Success 457
        | _ -> Failure [ "not 455" ]

        test <@ Success 455 |> Result.bind fn = Success 457 @>
        test <@ Success 566 |> Result.bind fn =  Failure [ "not 455" ] @>
        test <@ Failure [ "erro1"; "error2" ] |> Result.bind fn =  Failure [ "erro1"; "error2" ] @>

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
        test <@ fn 13 = None @>
        test <@ fn 321 = Some 508 @>
        test <@ fn 455 = None @>


    [<Fact>]
    let ``*``() =
        let fn1 = function
        | 321 -> Some 327
        | 327 -> Some 508
        | 455 -> Some 457
        | _ -> None

        let fn = fn1 * 2
        test <@ fn 13 = None @>
        test <@ fn 321 = Some 508 @>
        test <@ fn 455 = None @>


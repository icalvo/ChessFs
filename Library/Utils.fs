﻿module Utils

    module Option =
        let defaultTo defValue opt = defaultArg opt defValue
        let pipe action = Option.map (fun x -> action x; x)
        let mapList fn opt =
            opt
            |> Option.map fn 
            |> defaultTo []

    module Result =
        let defaultTo errorValue = function
        | Ok x -> x
        | Error _ -> errorValue

        let toValue = function
        | Ok x -> x
        | Error x -> x

        let toValue2 fn errorValue =
            Result.map fn >> defaultTo errorValue

    module Seq =
        let pipe action = Seq.map (fun x -> action x; x)
        let debug name = pipe (fun x -> if Configuration.debug then printfn $"%s{name} yields %A{x}")
        let filterNones (s: seq<'a option>): seq<'a> = Seq.choose id s
        let takeWhileIncludingLast predicate (s:seq<_>) = 
            /// Iterates over the enumerator, yielding elements and
            /// stops after an element for which the predicate does not hold
            let rec loop (en:System.Collections.Generic.IEnumerator<_>) = seq {
                if en.MoveNext() then
                    // Always yield the current, stop if predicate does not hold
                    yield en.Current
                    if predicate en.Current then
                        yield! loop en }

            // Get enumerator of the sequence and yield all results
            // (making sure that the enumerator gets disposed)
            seq { use en = s.GetEnumerator()
                yield! loop en }

        /// <summary>
        /// Returns a sequence that contains the elements generated by the given computation.
        /// </summary>
        /// <param name="nextFn"></param>
        /// <remarks>Works as Seq.unfold, but in this version the State and returned element are the same.
        /// Therefore, the generator function has the signature:
        ///     'State -> 'State option
        /// </remarks>
        let unfoldSimple nextFn =
            nextFn >> Option.map (fun x -> (x, x)) |> Seq.unfold

        /// Returns a sequence that yields chunks of length n.
        /// Each chunk is returned as a list.
        let batch length (xs: seq<'T>) =
            let rec loop xs =
                [
                    yield Seq.truncate length xs |> Seq.toList
                    match Seq.length xs <= length with
                    | false -> yield! loop (Seq.skip length xs)
                    | true -> ()
                ]
            loop xs

    module List =
        let pipe action = List.map (fun x -> action x; x)
        let debug name = pipe (printfn "%s yields %A" name)
        let apply arg fnlist = fnlist |> List.map (fun fn -> fn arg)
        let filterNones (s: 'a option list): 'a list = List.choose id s
        let repeat value n = List.init n (fun _ -> value)
        let repeatrev n value = List.init n (fun _ -> value)

    let (>?>) f1 f2 = f1 >> Option.bind f2

    let rec (*) f1 i =
        if i = 1 then f1 else f1 >> Option.bind (f1 * (i-1))

    module Map =
        let keys map = map |> Map.toList |> List.map (fun (k, _) -> k)
        let values map = map |> Map.toList |> List.map (fun (_, v) -> v)
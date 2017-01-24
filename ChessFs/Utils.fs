module Utils
    open Configuration

    module Option =
        let defaultTo defValue opt = defaultArg opt defValue
        let pipe action = Option.map (fun x -> action x; x)
        let mapList fn opt =
            opt
            |> Option.map fn 
            |> defaultTo []

    module Seq =
        let pipe action = Seq.map (fun x -> action x; x)
        let debug name = pipe (fun x -> if Configuration.debug then printfn "%s yields %A" name x)
        let filterNones (s: seq<'a option>): seq<'a> = Seq.choose id s
        let takeWhilePlusOne predicate (s:seq<_>) = 
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

    module List =
        let pipe action = List.map (fun x -> action x; x)
        let debug name = pipe (printfn "%s yields %A" name)
        let apply arg fnlist = fnlist |> List.map (fun fn -> fn arg)
        let filterNones (s: 'a option list): 'a list = List.choose id s

    type Result<'a> = 
    | Success of 'a
    | Failure of string list

    module Result = 

        let map f xResult = 
            match xResult with
            | Success x ->
                Success (f x)
            | Failure errs ->
                Failure errs
        // Signature: ('a -> 'b) -> Result<'a> -> Result<'b>

        // "return" is a keyword in F#, so abbreviate it
        let retn x = 
            Success x
        // Signature: 'a -> Result<'a>

        let apply fResult xResult = 
            match fResult,xResult with
            | Success f, Success x ->
                Success (f x)
            | Failure errs, Success x ->
                Failure errs
            | Success f, Failure errs ->
                Failure errs
            | Failure errs1, Failure errs2 ->
                // concat both lists of errors
                Failure (List.concat [errs1; errs2])
        // Signature: Result<('a -> 'b)> -> Result<'a> -> Result<'b>

        let bind f xResult = 
            match xResult with
            | Success x ->
                f x
            | Failure errs ->
                Failure errs

    let (>?>) f1 f2 = f1 >> Option.bind f2

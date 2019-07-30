module StateMachine
open System.Collections.Generic
open Chess
open Notation

let private takeWhileIncludingLast predicate (s:seq<_>) = 
  /// Iterates over the enumerator, yielding elements and
  /// stops after an element for which the predicate does not hold
  let rec loop (en:IEnumerator<_>) = seq {
    if en.MoveNext() then
      // Always yield the current, stop if predicate does not hold
      yield en.Current
      if predicate en.Current then
        yield! loop en }

  // Get enumerator of the sequence and yield all results
  // (making sure that the enumerator gets disposed)
  seq { use en = s.GetEnumerator()
        yield! loop en }
let stateMachine (transition:'State -> 'Input -> 'State) (isFinish:'State -> bool) (initialState:'State) (input:seq<'Input>): 'State list =
    input
    |> Seq.scan transition initialState
    |> takeWhileIncludingLast (not << isFinish)
    |> Seq.toList

let chessTransition former input equality =
    match former with
    | Draw _ -> 
        former
    | LostByResignation _ -> 
        former
    | WonByCheckmate _ -> 
        former
    | PlayerMoved (_, availableActions) | DrawOffer (_, _, availableActions) | DrawDeclinement (_, _, availableActions) ->
        let executableAction = 
            availableActions
            |> List.tryFind (fun x -> equality x.action input)
        match executableAction with
        | Some x -> x.execute()
        | None -> raise (System.Exception(sprintf "Could not find action %A. Available: %A" input (availableActions |> List.map (fun x -> playerActionToAlgebraic x.action))))

let chessFinish = function
| PlayerMoved _ | DrawOffer _ | DrawDeclinement _ -> false
| _ -> true

let chessStateMachineFrom initial input equality =
    let trans x y = chessTransition x y equality
    stateMachine trans chessFinish initial input

let chessStateMachine input equality =
    let trans x y = chessTransition x y equality
    stateMachine trans chessFinish newChessGame input

let algebraicNotationEquality m (input:string) = (playerActionToAlgebraic m).ToLowerInvariant() = input.ToLowerInvariant()

let algebraicChessStateMachine input = chessStateMachine input algebraicNotationEquality
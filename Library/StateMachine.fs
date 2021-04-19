module StateMachine

open Utils

let stateMachine (transition:'State -> 'Input -> 'State) (isFinish:'State -> bool) (initialState:'State) (input:seq<'Input>): seq<'State> =
    input
    |> Seq.scan transition initialState
    |> Seq.takeWhileIncludingLast (not << isFinish)

let listedStateMachine (transition:'State -> 'Input -> 'State) (isFinish:'State -> bool) (initialState:'State) (input:seq<'Input>) =
    stateMachine transition isFinish initialState input |> Seq.toList
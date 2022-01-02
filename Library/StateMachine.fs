module StateMachine

open Utils

let stateMachine (transition:'State -> 'Input -> 'State) (isFinish:'State -> bool) (initialState:'State) (input:seq<'Input>): seq<'State> =
    input
    |> Seq.scan transition initialState
    |> Seq.takeWhileIncludingLast (not << isFinish)


let resultStateMachine (transition:'State -> 'Input -> Result<'State, 'TError>) (isFinish:'State -> bool) (initialState:'State) (input:seq<'Input>): seq<Result<'State, 'TError>> =
    let transition2 (stateResult: Result<'State, 'TError>) (inp: 'Input): Result<'State, 'TError> =
        let transitionForInput state = transition state inp
        Result.bind transitionForInput stateResult

    let isFinish2 = function
        | Ok x -> isFinish x
        | Error _ -> true

    stateMachine transition2 isFinish2 (Ok initialState) input

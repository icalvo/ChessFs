module StateMachine

open Utils

let stateMachine (transition:'State -> 'Input -> 'State) (isFinish:'State -> bool) (initialState:'State) (input:seq<'Input>): seq<'State> =
    input
    |> Seq.scan transition initialState
    |> Seq.takeWhileIncludingLast (not << isFinish)


let resultStateMachine (transition:'State -> 'Input -> Result<'State>) (isFinish:'State -> bool) (initialState:'State) (input:seq<'Input>): seq<Result<'State>> =
    let transition2 (stateResult: Result<'State>) (inp: 'Input): Result<'State> =
        let transitionForInput state = transition state inp
        Result.bind transitionForInput stateResult

    let isFinish2 = function
        | Success x -> isFinish x
        | Failure _ -> true

    stateMachine transition2 isFinish2 (Success initialState) input

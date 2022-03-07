module StateMachine

open Utils

type StateMachine<'Input,'State> = seq<'Input> -> seq<'State>

let stateMachine (transition:'State -> 'Input -> 'State) (isFinish:'State -> bool) (initialState:'State): StateMachine<'Input, 'State> =
    Seq.scan transition initialState >> Seq.takeWhileIncludingLast (not << isFinish)

let resultStateMachine (transition:'State -> 'Input -> Result<'State, 'TError>) (isFinish:'State -> bool) (initialState:'State): StateMachine<'Input, Result<'State, 'TError>> =
    let transitionResult (stateResult: Result<'State, 'TError>) (inp: 'Input): Result<'State, 'TError> =
        let transitionForInput state = transition state inp
        Result.bind transitionForInput stateResult

    let isFinishResult = function
        | Ok x -> isFinish x
        | Error _ -> true

    stateMachine transitionResult isFinishResult (Ok initialState)

let last sm = sm >> Seq.last

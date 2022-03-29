module StateActions

open Utils
open StateMachine

let stateActionsFailingStateMachine initialState availableActions normalizeAction normalizeInput execute inputs =
    let transition state input =
        let availableActions = state |> availableActions
        let foundAction =
            availableActions
            |> List.tryFind (fun action -> (normalizeAction action) = (normalizeInput input))
        match foundAction with
        | Some action -> Ok ((execute action)())
        | None ->
            let actions = availableActions |> List.map normalizeAction |> String.concat ", "
            Error $"Could not find action {input}. Available: {actions}"

    let isFinish = availableActions >> List.isEmpty

    resultStateMachine transition isFinish initialState inputs

let actionsTransitionIgnoring availableActions normalizeAction normalizeInput execute state input =
    let foundAction =
        state
        |> availableActions
        |> List.tryFind (fun action -> (normalizeAction action) = (normalizeInput input))
    match foundAction with
    | Some action -> (execute action)()
    | None -> state

let stateActionsIgnoringStateMachine initialState availableActions normalizeAction normalizeInput execute inputs =
    let transition = actionsTransitionIgnoring availableActions normalizeAction normalizeInput execute
    let isFinish = availableActions >> List.isEmpty

    stateMachine transition isFinish initialState inputs
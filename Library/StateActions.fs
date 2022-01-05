module StateActions

open Utils
open StateMachine

let stateActionsFailingStateMachine initialState actions normalizeAction normalizeInput execute input =
    let transition sa i =
        let availableActions = sa |> actions
        let foundAction =
            availableActions
            |> List.tryFind (fun x -> (x |> normalizeAction) = (i |> normalizeInput))
        match foundAction with
        | Some action -> Ok ((execute action)())
        | None -> Error $"Could not find action %A{i}."

    let isFinish = actions >> List.isEmpty

    resultStateMachine transition isFinish initialState input

let actionsTransitionIgnoring actions normalizeAction normalizeInput execute sa i =
    let foundAction =
               sa
               |> actions
               |> List.tryFind (fun x -> (x |> normalizeAction) = (i |> normalizeInput))
    match foundAction with
    | Some action -> (execute action)()
    | None -> sa

let stateActionsIgnoringStateMachine initialState actions normalizeAction normalizeInput execute input =
    let transition = actionsTransitionIgnoring actions normalizeAction normalizeInput execute
    let isFinish = actions >> List.isEmpty

    stateMachine transition isFinish initialState input
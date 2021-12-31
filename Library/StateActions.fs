module StateActions

open Utils
open StateMachine

let stateActionsFailingStateMachine initialState actions normalizeAction normalizeInput execute input =
    let transition sa i =
        let availableActions = sa |> actions
        let action =
            availableActions
            |> List.tryFind (fun x -> (x |> normalizeAction) = (i |> normalizeInput))
        match action with
        | Some x -> Success ((execute x)())
        | None -> Failure (sa, [ $"Could not find action %A{i}." ])

    let isFinish = actions >> List.isEmpty

    resultStateMachine transition isFinish initialState input

let stateActionsIgnoringStateMachine initialState actions normalizeAction normalizeInput execute input =
    let transition sa i =
        let executableAction = 
                   sa
                   |> actions
                   |> List.tryFind (fun x -> (x |> normalizeAction) = (i |> normalizeInput))
        match executableAction with
        | Some x -> (execute x)()
        | None -> sa

    let isFinish = actions >> List.isEmpty

    stateMachine transition isFinish initialState input
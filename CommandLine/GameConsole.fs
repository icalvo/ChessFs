module GameConsole

open System

type GameState<'a> =
| AskingAction of 'a
| AskingToPlayAgain
| Exiting


let findExecutableAction (normalizeAction: 'T -> string) (input: Lazy<string>) =
    Seq.tryFind (fun action -> (normalizeAction action) = input.Force().ToLowerInvariant())

let findActionResult input availableActions execute translateAction = 
    let foundAction = findExecutableAction translateAction input availableActions

    foundAction |> Option.bind (fun action -> Some ((execute action)()))

/// Given that the user has not quit, attempt to parse
/// the input text into a index and then find the move
/// corresponding to that index
let askActionResult (input: Lazy<string>) availableActions oldActionResult execute translateAction = 
    match (findActionResult input availableActions execute translateAction) with
    | Some outcome -> outcome
    | None ->
        printfn $"...%s{input.Force()} is not a valid move. Try again"
        oldActionResult

let askToPlayAgain newGame (input: Lazy<string>) =
    printfn "Would you like to play again (y/n)?"
    match input.Force() with
    | "y" -> 
        AskingAction newGame
    | "n" -> 
        Exiting
    | _ ->
        AskingToPlayAgain

let gameConsoleTransition2 newGame actions execute translateAction printOutcome state (input: Lazy<string>) =
    match state with
    | AskingAction a ->
        let availableActions = actions a
        printfn "Enter an action or q to quit:"
        match input.Force() with
        | "q" -> Exiting
        | _   ->
            let newActionResult = askActionResult input availableActions a execute translateAction
            printOutcome newActionResult
            AskingAction newActionResult
    | AskingToPlayAgain ->
        askToPlayAgain newGame input
    | Exiting ->
        Exiting

let consoleInput =
    Seq.initInfinite (fun _ -> lazy(Console.ReadLine()))
    |>  Seq.takeWhile ((<>) null)


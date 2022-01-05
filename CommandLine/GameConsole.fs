module GameConsole

open System

type GameState<'a> =
| AskingAction of 'a
| AskingToPlayAgain
| Exiting


let private tryFindExecutableAction (normalizeAction: 'T -> string) normalizeInput (input: string) =
    Seq.tryFind (fun action -> normalizeAction action = normalizeInput input)

/// Given that the user has not quit, attempt to parse
/// the input text into a index and then find the move
/// corresponding to that index
let private askActionAndTryToExecute input availableActions gameState executefn normalizeAction normalizeInput =
    let foundAction = availableActions |> Seq.tryFind (fun action -> normalizeAction action = normalizeInput input)
    match foundAction with
    | Some action ->
        (executefn action)()
    | None ->
        printfn $"...%s{input} is not a valid action. Try again."
        gameState

let gameConsoleTransition newGame actions executefn normalizeAction normalizeInput printOutcome printHelp consoleState (input: Lazy<string>) =
    match consoleState with
    | AskingAction gameState ->
        let availableActions = gameState |> actions
        if List.isEmpty availableActions then
            AskingToPlayAgain
        else
            printOutcome gameState
            printfn "Enter an action, q to quit or h for showing help:"
            let inp = input.Force()
            match inp with
            | "q" -> Exiting
            | "h" ->
                printHelp()
                consoleState
            | _   ->
                let newGameState = askActionAndTryToExecute inp availableActions gameState executefn normalizeAction normalizeInput
                AskingAction newGameState
    | AskingToPlayAgain ->
        printfn "Would you like to play again (y/n)?"
        match input.Force() with
        | "y" -> 
            AskingAction newGame
        | "n" -> 
            Exiting
        | _ ->
            AskingToPlayAgain
    | Exiting ->
        Exiting

let consoleInput =
    Seq.initInfinite (fun _ -> lazy(Console.ReadLine()))
    |>  Seq.takeWhile ((<>) null)

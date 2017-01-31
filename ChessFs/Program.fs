open Domain
open Output
open System

/// Track the UI state
type UserAction<'a> =
    | ContinuePlay of 'a
    | ExitGame

type ProgramState =
    | AskingAction of PlayerActionOutcome
    | AskingToPlayAgain
    | Exiting
 
let askExecutableAction input availableActions =
        availableActions
        |> List.filter (fun { action = m } -> playerActionToString m = input)
        |> List.tryHead

/// Given that the user has not quit, attempt to parse
/// the input text into a index and then find the move
/// corresponding to that index
let askActionResult input availableActions oldActionResult = 
    match (askExecutableAction input availableActions) with
    | Some executableAction ->
        executableAction.execute()  
    | None ->
        printfn "...%s is not a valid move. Try again" input
        // displayInfo |> displayCells
        oldActionResult

/// Ask the user for input. Process the string entered as 
/// a move index or a "quit" command
let askProgramAction availableActions input actionResult = 
    printfn "Enter an int corresponding to a displayed move or q to quit:"
    if input = "q" then
        Exiting
    else
        let newActionResult = askActionResult input availableActions actionResult
        printOutcome newActionResult
        AskingAction newActionResult
  
let handleActionResult formerPlayerActionResult input =
    match formerPlayerActionResult with
    | Draw _ -> 
        AskingToPlayAgain
    | WonByAbandon _ -> 
        AskingToPlayAgain
    | WonByCheckMate _ -> 
        AskingToPlayAgain
    | PlayerMoved (_, availableActions) -> 
        askProgramAction availableActions input formerPlayerActionResult

let askToPlayAgain input =
    printfn "Would you like to play again (y/n)?"
    match input with
    | "y" -> 
        AskingAction (newGame())
    | "n" -> 
        Exiting
    | _ ->
        AskingToPlayAgain

let transition state input =
    match state with
    | AskingAction a ->
        handleActionResult a input
    | AskingToPlayAgain ->
        askToPlayAgain input
    | Exiting ->
        Exiting

let gameLoop input =
    let initialState = AskingAction (newGame())
    let isNotExiting = function
        | Exiting -> false
        | _ -> true

    input
    |> Seq.scan transition initialState
    |> Seq.takeWhile isNotExiting
    |> Seq.iter ignore

let interactiveConsole (transition:'State -> 'Input -> 'State) (isFinish:'State -> bool) (initialState:'State) (input:seq<'Input>): 'State list =
    input
    |> Seq.scan transition initialState
    |> Seq.takeWhile (not << isFinish)
    |> Seq.toList

let consoleInput =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |>  Seq.takeWhile ((<>) null)

[<EntryPoint>]
let main argv =
    let firstActionResult = newGame()
    printOutcome firstActionResult
    let initialState = AskingAction firstActionResult
    let isFinish = function
        | Exiting -> true
        | _ -> false

    let input = if Array.isEmpty argv then consoleInput else Array.toSeq argv

    ignore <| interactiveConsole transition isFinish initialState input
    printfn "Bye!"
    ignore <| Console.ReadLine()
    0
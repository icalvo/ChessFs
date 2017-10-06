open Chess
open Output
open System
open GameConsole

let askExecutableAction input availableActions =
        availableActions
        |> List.filter (fun { action = m } -> playerActionToAlgebraic m = input)
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
 
/// Ask the user for a draw agreement.
let askDrawAgreement input displayInfo player playerMovementCapabilities = 
    printfn "Draw offered. Enter y to accept, n to reject or q to quit:"
    match input with
    | "y" -> AskingAction (Draw (displayInfo, player))
    | "n" -> AskingAction (PlayerMoved (displayInfo, playerMovementCapabilities))
    | "q" -> Exiting
    | _   -> Exiting
  
let handleChessActionOutcome formerPlayerActionOutcome input =
    match formerPlayerActionOutcome with
    | Draw _ -> 
        AskingToPlayAgain
    | LostByResignation _ -> 
        AskingToPlayAgain
    | WonByCheckmate _ -> 
        AskingToPlayAgain
    | PlayerMoved (_, availableActions) -> 
        askProgramAction availableActions input formerPlayerActionOutcome
    | DrawOffer (displayInfo, player, playerMovementCapabilities) ->
        askDrawAgreement input displayInfo player playerMovementCapabilities


[<EntryPoint>]
let main argv =
    printOutcome newChessGame
    let initialState = AskingAction newChessGame
    let isFinish = function
        | Exiting -> true
        | _ -> false

    let input = if Array.isEmpty argv then consoleInput else Array.toSeq argv
    let chessTransition = gameTransition newChessGame handleChessActionOutcome

    ignore <| interactiveConsole chessTransition isFinish initialState input
    printfn "Bye!"
    ignore <| Console.ReadLine()
    0
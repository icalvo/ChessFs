open System
open Chess
open Output
open GameConsole

let findExecutableAction input availableActions =
        availableActions
        |> List.filter (fun { action = m } -> playerActionToAlgebraic m = input)
        |> List.tryHead

let findActionResult input availableActions = 
    let foundAction = findExecutableAction input availableActions

    foundAction |> Option.bind (fun action -> Some (action.execute()))

let next algebraicMove =
    function
    | Some (PlayerMoved (_, availableActions)) ->
            findActionResult algebraicMove availableActions
    | _ -> None

let next2 game algebraicMove = next algebraicMove game

/// Given that the user has not quit, attempt to parse
/// the input text into a index and then find the move
/// corresponding to that index
let askActionResult input availableActions oldActionResult = 
    match (findActionResult input availableActions) with
    | Some outcome -> outcome
    | None ->
        printfn "...%s is not a valid move. Try again" input
        oldActionResult

/// Ask the user for input. Process the string entered as 
/// a move index or a "quit" command
let askProgramAction availableActions input actionResult = 
    printfn "Enter an action or q to quit:"

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
    let input = ["e4";"e5";"f3";"Qh4"]
    let game2 =
        input
        |> List.fold next2 (Some newChessGame)
        |> Option.get

    printOutcome game2
    let initialState = AskingAction game2
    let isFinish = function
        | Exiting -> true
        | _ -> false
    

        //|> Seq.append argv
    let input = consoleInput

    let chessTransition = gameTransition game2 handleChessActionOutcome

    ignore <| stateMachine chessTransition isFinish initialState input
    printfn "Bye!"
    ignore <| Console.ReadLine()
    0
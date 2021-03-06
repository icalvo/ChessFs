﻿open System
open Chess
open Notation
open Output
open GameConsole

let findExecutableAction (input: Lazy<string>) availableActions =
        availableActions
        |> List.tryFind (fun { action = m } -> (playerActionToAlgebraic m).ToLowerInvariant() = input.Force().ToLowerInvariant())

let findActionResult input availableActions = 
    let foundAction = findExecutableAction input availableActions

    foundAction |> Option.bind (fun action -> Some (action.execute()))

let next algebraicMove =
    function
    | Some (PlayerMoved (_, availableActions)) ->
            findActionResult algebraicMove availableActions
    | _ -> None

/// Given that the user has not quit, attempt to parse
/// the input text into a index and then find the move
/// corresponding to that index
let askActionResult input availableActions oldActionResult = 
    match (findActionResult input availableActions) with
    | Some outcome -> outcome
    | None ->
        printfn "...%s is not a valid move. Try again" (input.Force())
        oldActionResult

/// Ask the user for input. Process the string entered as 
/// a move index or a "quit" command
let askProgramAction availableActions (input: Lazy<string>) actionResult = 
    printfn "Enter an action or q to quit:"
    match input.Force() with
    | "q" -> Exiting
    | _   ->
        let newActionResult = askActionResult input availableActions actionResult
        printOutcome newActionResult
        AskingAction newActionResult

let handleChessActionOutcome formerPlayerActionOutcome (input: Lazy<string>) =
    match formerPlayerActionOutcome with
    | Draw _ -> 
        AskingToPlayAgain
    | LostByResignation _ -> 
        AskingToPlayAgain
    | WonByCheckmate _ -> 
        AskingToPlayAgain
    | PlayerMoved (_, availableActions)
    | DrawOffer (_, _, availableActions)
    | DrawDeclinement (_, _, availableActions) -> 
        askProgramAction availableActions input formerPlayerActionOutcome

[<EntryPoint>]
let main argv =
    let game = newChessGame

    printOutcome game
    let initialState = AskingAction game
    let isFinish = function
        | Exiting -> true
        | _ -> false

    // let scholarsMate = "e4 e5 Bc4 Nc6 Qh5 Nf6 Qxf7".Split(' ')
    // let extraInput = scholarsMate;
    let extraInput = argv |> Seq.map (fun x -> lazy(x));

    let input = Seq.append extraInput consoleInput

    let chessTransition = gameTransition game handleChessActionOutcome

    ignore <| StateMachine.stateMachine chessTransition isFinish initialState input
    printfn "Bye!"
    0
open Domain
open Output
open System

/// Track the UI state
type UserAction<'a> =
    | ContinuePlay of 'a
    | ExitGame

///<summary>Print each available move on the console</summary>
let displayActions domainActions = 
    domainActions
    |> Seq.map (fun moveInfo -> playerActionToString moveInfo.action)
    |> String.concat ", "
    |> printfn "%s"
 
/// <summary>Display the cells on the console in a grid</summary>
let displayCells displayInfo = 
    board cellStateToString displayInfo |> printBoard
    printfn ""   // add some space

type ProgramState =
    | AskingAction of PlayerActionResult
    | AskingToPlayAgain
    | Exiting

let askDomainAction input availableActions =
        availableActions
        |> List.filter (fun { action = m } -> playerActionToString m = input)
        |> List.tryHead

/// Given that the user has not quit, attempt to parse
/// the input text into a index and then find the move
/// corresponding to that index
let tryExecuteAction input availableActions formerMoveResult = 
    let chosenAction = askDomainAction input availableActions

    match chosenAction with
    | Some action ->
        let moveResult = action.execute()  
        AskingAction moveResult
    | None ->
        printfn "...%s is not a valid move. Try again" input
        // displayInfo |> displayCells
        AskingAction formerMoveResult

/// Ask the user for input. Process the string entered as 
/// a move index or a "quit" command
let askProgramAction availableCapabilities input moveResult = 
    printfn "Enter an int corresponding to a displayed move or q to quit:" 
    if input = "q" then
        Exiting
    else
        tryExecuteAction input availableCapabilities moveResult

let printActionResult formerPlayerActionResult =
    // handle each case of the result
    match formerPlayerActionResult with
    | Draw (displayInfo, _) -> 
        displayInfo |> displayCells
        printfn "GAME OVER - Draw"
        printfn ""
    | WonByAbandon (displayInfo, player) -> 
        displayInfo |> displayCells
        printfn "GAME WON because %A abandoned" (opponent player)
        printfn ""
    | WonByCheckMate (displayInfo, player) -> 
        displayInfo |> displayCells
        printfn "GAME WON by %A's checkmate" player
        printfn ""
    | PlayerWhiteToMove (displayInfo, nextMoves) -> 
        displayInfo |> displayCells
        printfn "White to move" 
        displayActions nextMoves
    | PlayerBlackToMove (displayInfo, nextMoves) -> 
        displayInfo |> displayCells
        printfn "Black to move" 
        displayActions nextMoves
    
let handleActionResult formerPlayerActionResult input =
    printActionResult formerPlayerActionResult
    match formerPlayerActionResult with
    | Draw _ -> 
        AskingToPlayAgain
    | WonByAbandon _ -> 
        AskingToPlayAgain
    | WonByCheckMate _ -> 
        AskingToPlayAgain
    | PlayerWhiteToMove (_, nextMoves) -> 
        askProgramAction nextMoves input formerPlayerActionResult
    | PlayerBlackToMove (_, nextMoves) -> 
        askProgramAction nextMoves input formerPlayerActionResult

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
let main _ =
    let firstActionResult = newGame()
    printActionResult firstActionResult
    let initialState = AskingAction firstActionResult
    let isFinish = function
        | Exiting -> true
        | _ -> false

    let input =
        [ "e4"; "e5 "]

    ignore <| interactiveConsole transition isFinish initialState input

    0
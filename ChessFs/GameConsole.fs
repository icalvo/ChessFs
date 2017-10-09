module GameConsole

open System

let stateMachine2 (transition:'State -> (unit -> 'Input) -> 'State) (isFinish:'State -> bool) (initialState:'State) (inputfn:unit -> 'Input): 'State =
    transition initialState inputfn

let stateMachine (transition:'State -> 'Input -> 'State) (isFinish:'State -> bool) (initialState:'State) (input:seq<'Input>): 'State list =
    input
    |> Seq.scan transition initialState
    |> Seq.takeWhile (not << isFinish)
    |> Seq.toList

type GameState<'a> =
| AskingAction of 'a
| AskingToPlayAgain
| Exiting

let askToPlayAgain newGame input =
    printfn "Would you like to play again (y/n)?"
    match input with
    | "y" -> 
        AskingAction newGame
    | "n" -> 
        Exiting
    | _ ->
        AskingToPlayAgain

let gameTransition newGame handle state input =
    match state with
    | AskingAction a ->
        handle a input
    | AskingToPlayAgain ->
        askToPlayAgain newGame input
    | Exiting ->
        Exiting

let consoleInput2 () = Console.ReadLine()

let consoleInput =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |>  Seq.takeWhile ((<>) null)


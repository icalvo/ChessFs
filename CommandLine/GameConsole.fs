module GameConsole

open System

type GameState<'a> =
| AskingAction of 'a
| AskingToPlayAgain
| Exiting

let askToPlayAgain newGame (input: Lazy<string>) =
    printfn "Would you like to play again (y/n)?"
    match input.Force() with
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

let consoleInput =
    Seq.initInfinite (fun _ -> lazy(Console.ReadLine()))
    |>  Seq.takeWhile ((<>) null)


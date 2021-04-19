module ChessStateMachine

open StateMachine
open Chess
open Notation
open Utils

let chessTransition2 former input equality =
    match former with
    | Success former2 ->
        match former2 with
        | Draw _ -> 
            former
        | LostByResignation _ -> 
            former
        | WonByCheckmate _ -> 
            former
        | PlayerMoved (_, availableActions) | DrawOffer (_, _, availableActions) | DrawDeclinement (_, _, availableActions) ->
            let executableAction = 
                availableActions
                |> List.tryFind (fun x -> equality x.action input)
            match executableAction with
            | Some x -> Success (x.execute())
            | None -> Failure [ sprintf "Could not find action %A. Available: %A" input (availableActions |> List.map (fun x -> playerActionToAlgebraic x.action)) ]
    | Failure _ -> former
        
let chessTransition former input equality =
    match former with
    | Draw _ -> 
        former
    | LostByResignation _ -> 
        former
    | WonByCheckmate _ -> 
        former
    | PlayerMoved (_, availableActions) | DrawOffer (_, _, availableActions) | DrawDeclinement (_, _, availableActions) ->
        let executableAction = 
            availableActions
            |> List.tryFind (fun x -> equality x.action input)
        match executableAction with
        | Some x -> x.execute()
        | None -> raise (System.Exception(sprintf "Could not find action %A. Available: %A" input (availableActions |> List.map (fun x -> playerActionToAlgebraic x.action))))

let chessFinish = function
| WonByCheckmate _ | LostByResignation _ | Draw _ -> true
| _ -> false

let chessFinish2 = function
| Success x ->
    match x with
    | WonByCheckmate _ | LostByResignation _ | Draw _ -> true
    | _ -> false
| Failure _ -> true

let chessStateMachineFrom initial input equality =
    let trans x y = chessTransition x y equality
    stateMachine trans chessFinish initial input

let chessStateMachine input equality =
    let trans x y = chessTransition x y equality
    stateMachine trans chessFinish newChessGame input

let algebraicNotationEquality m (input:string) = (playerActionToAlgebraic m).ToLowerInvariant() = input.ToLowerInvariant()

let algebraicChessStateMachine input = chessStateMachine input algebraicNotationEquality
open Utils
open Domain
open Output
open System

//// TESTING
//let game1: GameState = [
//    cell White Bishop (A, R1)
//]

//let game2: GameState = [
//    cell White Bishop (A, R1)
//    cell White Knight (C, R3)
//]

//let game3 = 
//    init
//    |> moveAndReplace (E, R2) (E, R4)
//    |> moveAndReplace (C, R8) (H, R3)


/// Track the UI state
type UserAction<'a> =
    | ContinuePlay of 'a
    | ExitGame


/// Print each available move on the console
let displayNextMoves nextMoves = 
    nextMoves 
    |> List.iteri (fun i moveInfo -> 
        printfn "%i) %A" i moveInfo.movement)

/// Get the move corresponding to the 
/// index selected by the user
let getCapability selectedIndex nextMoves = 
    if selectedIndex < List.length nextMoves then
        let move = List.item selectedIndex nextMoves
        Some move.capability 
    else
        None

/// Given that the user has not quit, attempt to parse
/// the input text into a index and then find the move
/// corresponding to that index
let processMoveIndex inputStr availableMoves processInputAgain = 
    match Int32.TryParse inputStr with
    // TryParse will output a tuple (parsed?,int)
    | true,inputIndex ->
        // parsed ok, now try to find the corresponding move
        match getCapability inputIndex availableMoves with
        | Some capability -> 
            // corresponding move found, so make a move
            let moveResult = capability()  
            ContinuePlay moveResult // return it
        | None ->
            // no corresponding move found
            printfn "...No move found for inputIndex %i. Try again" inputIndex 
            // try again
            processInputAgain()
    | false, _ -> 
        // int was not parsed
        printfn "...Please enter an int corresponding to a displayed move."
        // try again
        processInputAgain()

/// Ask the user for input. Process the string entered as 
/// a move index or a "quit" command
let rec processInput availableCapabilities = 

    // helper that calls this function again with exactly
    // the same parameters
    let processInputAgain() = 
        processInput availableCapabilities 

    printfn "Enter an int corresponding to a displayed move or q to quit:" 
    let inputStr = Console.ReadLine()
    if inputStr = "q" then
        ExitGame
    else
        processMoveIndex inputStr availableCapabilities processInputAgain
            
/// <summary>Display the cells on the console in a grid</summary>
let displayCells displayInfo = 
    board cellStateToString displayInfo |> printBoard
    printfn ""   // add some space
        
/// After each game is finished,
/// ask whether to play again.
let rec askToPlayAgain i = 
    printfn "Would you like to play again (y/n)?"
    match Console.ReadLine() with
    | "y" -> 
        ContinuePlay (newGame())
    | "n" -> 
        ExitGame
    | _ -> askToPlayAgain i


/// The main game loop, repeated
/// for each user input
let rec gameLoop userAction = 
    printfn "\n------------------------------\n"  // a separator between moves
        
    match userAction with
    | ExitGame -> 
        printfn "Exiting game."
    | ContinuePlay moveResult -> 
        // handle each case of the result
        match moveResult with
        | Draw (displayInfo, player) -> 
            displayInfo |> displayCells
            printfn "GAME OVER - Draw"
            printfn ""             
            let nextUserAction = askToPlayAgain 1
            gameLoop nextUserAction
        | WonByAbandon (displayInfo, player) -> 
            displayInfo |> displayCells
            printfn "GAME WON becase %A abandoned" (otherPlayer player)
            printfn ""
            let nextUserAction = askToPlayAgain 1
            gameLoop nextUserAction
        | WonByCheckMate (displayInfo, player) -> 
            displayInfo |> displayCells
            printfn "GAME WON by %A's checkmate" player
            printfn ""
            let nextUserAction = askToPlayAgain 1
            gameLoop nextUserAction
        | PlayerWhiteToMove (displayInfo, nextMoves) -> 
            displayInfo |> displayCells
            printfn "White to move" 
            displayNextMoves nextMoves
            let newResult = processInput nextMoves
            gameLoop newResult 
        | PlayerBlackToMove (displayInfo, nextMoves) -> 
            displayInfo |> displayCells
            printfn "Black to move" 
            displayNextMoves nextMoves
            let newResult = processInput nextMoves
            gameLoop newResult 

[<EntryPoint>]
let main argv =
    let userAction = ContinuePlay (newGame())
    gameLoop userAction
    0
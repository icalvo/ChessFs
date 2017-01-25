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


///<summary>Print each available move on the console</summary>
let displayNextMoves nextMoves = 
    nextMoves
    |> Seq.map (fun moveInfo -> playerActionToString moveInfo.movement)
    |> String.concat ", "
    |> printfn "%s"

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

    let chosenMove = 
        availableMoves
        |> List.map (fun moveInfo -> (playerActionToString moveInfo.movement, moveInfo))
        |> List.filter (fun x -> fst x = inputStr)
        |> List.tryHead

    match chosenMove with
    | Some (_, move) ->
        // corresponding move found, so make a move
        let moveResult = move.capability()  
        ContinuePlay moveResult // return it
    | None ->
        // no corresponding move found
        printfn "...%s is not a valid move. Try again" inputStr
        // displayInfo |> displayCells
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
let rec askToPlayAgain() =
    printfn "Would you like to play again (y/n)?"
    match Console.ReadLine() with
    | "y" -> 
        ContinuePlay (newGame())
    | "n" -> 
        ExitGame
    | _ -> askToPlayAgain()


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
            let nextUserAction = askToPlayAgain()
            gameLoop nextUserAction
        | WonByAbandon (displayInfo, player) -> 
            displayInfo |> displayCells
            printfn "GAME WON because %A abandoned" (opponent player)
            printfn ""
            let nextUserAction = askToPlayAgain()
            gameLoop nextUserAction
        | WonByCheckMate (displayInfo, player) -> 
            displayInfo |> displayCells
            printfn "GAME WON by %A's checkmate" player
            printfn ""
            let nextUserAction = askToPlayAgain()
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
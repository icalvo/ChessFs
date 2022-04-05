namespace ChessFs.Chess.Engine

open ChessFs.Common
open ChessFs.Chess

module CheckStatus =
    let create =
        function
        | true -> IsCheck
        | false -> NoCheck
    let toBool =
        function
        | IsCheck -> true
        | NoCheck -> false

module PieceReaches =
    open Ply
    open Option
    open Coordinate

    // Directions
    let Up        = nextRank
    let UpRight   = nextFile >?> nextRank
    let Right     = nextFile
    let DownRight = nextFile >?> prevRank
    let Down      = prevRank
    let DownLeft  = prevFile >?> prevRank
    let Left      = prevFile
    let UpLeft    = prevFile >?> nextRank

    let noReaches: Coordinate -> PieceReaches = fun _ -> []

    let bishopReaches coordinate: PieceReaches =
        [ UpRight; DownRight; DownLeft; UpLeft ]
        |> List.map Seq.unfoldSimple
        |> List.apply coordinate

    let rookReaches coordinate: PieceReaches =
        [ Up; Right; Down; Left ]
        |> List.map Seq.unfoldSimple
        |> List.apply coordinate

    let queenReaches coordinate: PieceReaches =
        [ bishopReaches; rookReaches ]
        |> List.apply coordinate
        |> List.concat

    let kingReaches coordinate: PieceReaches =
        [ UpRight; DownRight; DownLeft; UpLeft; Up; Right; Down; Left ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let knightReaches coordinate: PieceReaches =
        [
            Up >?> Up >?> Left;
            Up >?> Up >?> Right;
            Down >?> Down >?> Left;
            Down >?> Down >?> Right;
            Right >?> Right >?> Up;
            Right >?> Right >?> Down;
            Left >?> Left >?> Up;
            Left >?> Left >?> Down;
        ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let pawnSingleMoveReaches pawnDirection coordinate: PieceReaches =
        match pawnDirection coordinate with
        | Some newCoordinate -> newCoordinate |> Seq.singleton |> List.singleton
        | None -> []

    let pawnDoubleMoveReaches pawnDirection coordinate: PieceReaches =
        Seq.unfoldSimple pawnDirection coordinate
        |> Seq.take 2
        |> List.singleton

    let pawnCaptureReaches pawnDirection coordinate: PieceReaches =
        [
            pawnDirection >?> Left;
            pawnDirection >?> Right;
        ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToKingReach castlingDirection coordinate: PieceReaches =
        [ castlingDirection * 2; ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    let kingCastleToQueenReach castlingDirection coordinate: PieceReaches =
        [ castlingDirection * 3; ]
        |> List.apply coordinate
        |> List.filterNones
        |> List.map Seq.singleton

    type PawnRankType =
        | PromotionRank
        | EnPassantRank
        | DoubleMoveRank
        | RegularRank

    let pieceReaches piece coordinate =
        let applyReaches (reachesFunc: Coordinate -> PieceReaches, act: PlyConstructor) =
            coordinate
            |> reachesFunc
            |> List.where (not << Seq.isEmpty)
            |> List.map (Seq.map (act coordinate piece))

        let collectReaches = List.collect applyReaches

        match piece with
        | Piece (color, Pawn) ->
            let _, rank = coordinate
            let direction = match color with | White -> Up | Black -> Down

            let pawnRankType =
                match (color, rank) with
                | White, R7 | Black, R2 -> PromotionRank
                | White, R5 | Black, R4 -> EnPassantRank
                | White, R2 | Black, R7 -> DoubleMoveRank
                | _ -> RegularRank

            match pawnRankType with
            | PromotionRank ->
                let plyAndReachesFuncs = [
                    moveAndPromote, pawnSingleMoveReaches
                    captureAndPromote, pawnCaptureReaches 
                ]
                let promotionShapes = [ Queen; Rook; Knight; Bishop ]

                promotionShapes
                |> List.allPairs plyAndReachesFuncs
                |> List.map (fun ((plyFunc, reachesFunc), promotionShape) -> reachesFunc direction, plyFunc promotionShape)
                |> List.collect applyReaches
            | EnPassantRank ->
                collectReaches [
                    pawnSingleMoveReaches direction, move
                    pawnCaptureReaches    direction, capture
                    pawnCaptureReaches    direction, captureEnPassant]
            | DoubleMoveRank ->
                collectReaches [
                    pawnDoubleMoveReaches direction, move
                    pawnCaptureReaches    direction, capture
                ]
            | RegularRank ->
                collectReaches [
                    pawnSingleMoveReaches direction, move
                    pawnCaptureReaches    direction, capture
                ]
        | Piece (_, Knight)
        | Piece (_, Bishop)
        | Piece (_, Rook  )
        | Piece (_, Queen ) ->
                let reachesFunc =
                    match Piece.shape piece with
                    | Knight -> knightReaches
                    | Bishop -> bishopReaches
                    | Rook -> rookReaches
                    | Queen -> queenReaches
                    | _ -> fun _ -> []

                [ move; capture ]
                |> List.map (fun plyFunc -> reachesFunc, plyFunc)
                |> List.collect applyReaches            
        | Piece (color, King  ) ->
            let kingSideDirection = match color with | White -> Right | Black -> Left;
            let queenSideDirection = match color with | White -> Left | Black -> Right;
            collectReaches [
                (kingReaches, move)
                (kingReaches, capture)
                (kingCastleToKingReach  kingSideDirection , castleKingSide)
                (kingCastleToQueenReach queenSideDirection, castleQueenSide)
            ]

module ChessState =
    open Square
    open Coordinate
    open ChessState

    /// <summary>
    /// Can the ply type be legally executed for a piece with a given target coordinate?
    /// </summary>
    let internal executeReason game ply =
        seq {            
            let plyColor = Ply.color ply
            let targetCoordinate = Ply.target ply
            let targetSquare = game @@@ targetCoordinate
            let differentColor color square =
                match pieceColor square with
                | Some White -> color = Black
                | Some Black -> color = White
                | None -> true

            match ply with
            | Move _ | MoveAndPromote _ ->
                if targetSquare |> (not << isEmpty) then yield $"Cannot move to the occupied square %A{targetSquare}"
            | Capture _ | CaptureAndPromote _ ->
                if not (hasPiece targetSquare) then yield "Cannot capture an empty square"
                if not (differentColor plyColor targetSquare) then yield "Cannot capture own pieces"
            | CaptureEnPassant _ ->
                match game.pawnCapturableEnPassant with
                | Some capturable -> if not (capturable = targetCoordinate) then yield "Cannot capture en passant if the target is not the enabled pawn" 
                | None -> yield "Cannot capture en passant if there is no enabled pawn"
            | CastleKingSide color ->
                match color with
                | White ->
                    if not game.castlingRights.white.canCastleKingSide then yield "Right to castle kingside is lost"
                    if game @@@ F1 |> (not << isEmpty) then yield "F1 is not empty" 
                    if game @@@ G1 |> (not << isEmpty) then yield "G1 is not empty"
                | Black ->
                    if not game.castlingRights.black.canCastleKingSide then yield "Right to castle kingside is lost"
                    if game @@@ F8 |> (not << isEmpty) then yield "F1 is not empty" 
                    if game @@@ G1 |> (not << isEmpty) then yield "G1 is not empty"
                    if not game.castlingRights.black.canCastleKingSide then yield "Right to castle kingside is lost"
                    if game @@@ F8 |> (not << isEmpty) then yield "F8 is not empty"
                    if game @@@ G8 |> (not << isEmpty) then yield "G8 is not empty" 
            | CastleQueenSide color ->
                match color with
                | White ->
                    if not game.castlingRights.white.canCastleQueenSide then yield "Right to castle queenside is lost"
                    if game @@@ C1 |> (not << isEmpty) then yield "C1 is not empty" 
                    if game @@@ D1 |> (not << isEmpty) then yield "D1 is not empty" 
                | Black ->
                    if not game.castlingRights.black.canCastleQueenSide then yield "Right to castle queenside is lost"
                    if game @@@ C8 |> (not << isEmpty) then yield "C8 is not empty" 
                    if game @@@ D8 |> (not << isEmpty) then yield "D8 is not empty" 
        }

    /// <summary>
    /// Can the ply type be legally executed for a piece with a given target coordinate?
    /// </summary>
    /// <remarks>
    /// Attacks are ignored, so if the ply leaves the king in check or if someone is attacking
    /// the castling path in a castle ply (F1, F8, D1, D8), that is not considered. 
    /// </remarks>
    let internal canExecuteExceptForAttacks game ply =
        let plyColor = Ply.color ply
        let targetCoordinate = Ply.target ply
        let targetSquare = game @@@ targetCoordinate
        let differentColor color square =
            match pieceColor square with
            | Some White -> color = Black
            | Some Black -> color = White
            | None -> true

        let areAllTrueFor arg = Array.forall ((|>) arg)

        match ply with
        | Move _ | MoveAndPromote _ ->
            targetSquare |> isEmpty
        | Capture _ | CaptureAndPromote _ ->
            [| hasPiece; differentColor plyColor |]
            |> areAllTrueFor targetSquare
        | CaptureEnPassant _ ->
            match game.pawnCapturableEnPassant with
            | Some capturable -> capturable = targetCoordinate
            | None -> false
        | CastleKingSide color ->
            match color with
            | White ->
                game.castlingRights.white.canCastleKingSide &&
                game @@@ F1 |> isEmpty &&
                game @@@ G1 |> isEmpty
            | Black ->
                game.castlingRights.black.canCastleKingSide &&
                game @@@ F8 |> isEmpty &&
                game @@@ G8 |> isEmpty
        | CastleQueenSide color ->
            match color with
            | White ->
                game.castlingRights.white.canCastleQueenSide &&
                game @@@ B1 |> isEmpty &&
                game @@@ C1 |> isEmpty &&
                game @@@ D1 |> isEmpty
            | Black ->
                game.castlingRights.black.canCastleQueenSide &&
                game @@@ B8 |> isEmpty &&
                game @@@ C8 |> isEmpty &&
                game @@@ D8 |> isEmpty

module internal ReachEvaluation =
    open ChessState
    open Coordinate
    open System.Diagnostics
    open Square
    open Color
    open Player

    let internal evaluateReachSquare game ply =
        let canDoPly = canExecuteExceptForAttacks game ply
        let movePly = Ply.equivalentMove ply
        let canMove = canExecuteExceptForAttacks game movePly
        if canDoPly then
            if Ply.isCapture ply then
                PossibleFinalPly ply
            else
                PossiblePly ply
        // Even if the ply cannot be executed in the current target coordinate, if you can move through it we will
        // consider it an intermediate step that can still let us get to a good target square for the ply.
        elif canMove then
            Intermediate
        else
            Impossible

    let internal reachCapabilities game plies =
        let first = plies |> Seq.head
        
        let shape = first |> Ply.shape
        let color = first |> Ply.color
        let source = first |> Ply.source
        let isCapture = first |> Ply.isCapture
        if source = H5 && shape = Queen && color = White && isCapture then
            Debugger.Break()
        
        plies
        |> Seq.map (evaluateReachSquare game)
        |> Seq.takeWhileIncludingLast (not << ReachStep.isFinal)
        |> Seq.map ReachStep.ply
        |> Seq.filterNones

    /// <summary>Legal plies for a piece (without taking into account the
    /// out-of-check rule).</summary>
    let internal pieceCapabilitiesWithoutCheckFilter game piece sourceCoordinate =
        let pieceReaches = PieceReaches.pieceReaches piece sourceCoordinate
        pieceReaches |> Seq.collect (reachCapabilities game)

    /// <summary>Is the coordinate attacked by a player?</summary>
    let internal isAttackedBy playerColor game targetCoordinate =
        let attacksBy (coordinate, piece) =
            let direction = match playerColor with | White -> PieceReaches.Up | Black -> PieceReaches.Down
            let reachesFor: Coordinate -> PieceReaches =
                match Piece.shape piece with
                | Knight -> PieceReaches.knightReaches
                | Bishop -> PieceReaches.bishopReaches
                | Rook -> PieceReaches.rookReaches
                | Queen -> PieceReaches.queenReaches
                | Pawn -> PieceReaches.pawnCaptureReaches direction
                | _ -> PieceReaches.noReaches

            let reaches = reachesFor coordinate
            let traverseReach = Seq.takeWhileIncludingLast ((@@@) game >> isEmpty)
            reaches |> Seq.collect traverseReach

        let allReaches = [ PieceReaches.queenReaches; PieceReaches.knightReaches ]
        let potentialAttackSources =
            allReaches
            |> Seq.collect (fun x -> x targetCoordinate)
            |> Seq.collect id

        let hasPieceWithColor: Square -> bool = function
        | PieceSquare (Piece(color, _), _) -> color = playerColor
        | _ -> false

        let x: Square -> Coordinate seq =
            pieceAndCoordinate
            >> (Option.map attacksBy)
            >> (Option.defaultValue Seq.empty)
        let attackedCoordinates =
            potentialAttackSources
            |> Seq.map ((@@@) game)
            |> Seq.where hasPieceWithColor
            |> Seq.collect x
            
        Seq.contains targetCoordinate attackedCoordinates

    /// <summary>Executes a board change (unchecked).</summary>
    let private rawBoardChange pieces (boardChange: BoardChange) =
        match boardChange with
        | BoardChange.MovePiece (sourceCoordinate, targetCoordinate) ->
            match pieces @@ sourceCoordinate with
            | EmptySquare _ ->
                failwith $"Trying to move a piece at %A{sourceCoordinate} but there is no piece there."
            | PieceSquare (piece, _) ->
                pieces
                |> Map.remove sourceCoordinate
                |> Map.remove targetCoordinate
                |> Map.add targetCoordinate piece
        | RemovePiece coordinate ->
            assert (pieces |> Map.containsKey coordinate)

            pieces
            |> Map.remove coordinate
        | AddPiece (col, shp, coordinate) ->
            pieces
            |> Map.remove coordinate
            |> Map.add coordinate (Piece (col, shp))

    /// <summary>Is the player in check?</summary>
    let internal isCheck player (game: ChessState) =
        let (Player playerColor) = player
        let kingCoordinate = game.king player
        isAttackedBy (opposite playerColor) game kingCoordinate |> CheckStatus.create

    let check g = isCheck g.playerInTurn g

    let private rawExecutePly ply pieces =
        ply
        |> Ply.boardChanges
        |> List.fold rawBoardChange pieces

    let internal nextGameState ply restOfPlies drawOffer gameState =
        let plyPreventsWhiteCastleKingSide =
            match ply with
            | CastleKingSide White
            | CastleQueenSide White
            | Move    (Piece (White, King), _, _)
            | Move    (Piece (White, Rook), (H, R1), _)
            | Capture (Piece (White, Rook), _, _)
            | Capture (Piece (White, King), _, _)
            | Capture (_, _, (H, R1)) -> true
            | _ -> false
        let plyPreventsBlackCastleKingSide =
            match ply with
            | CastleKingSide Black
            | CastleQueenSide Black
            | Move    (Piece (Black, King), _, _)
            | Move    (Piece (Black, Rook), (H, R8), _)
            | Capture (Piece (Black, Rook), _, _)
            | Capture (Piece (Black, King), _, _)
            | Capture (_, _, (H, R8)) -> true
            | _ -> false
        let plyPreventsWhiteCastleQueenSide =
            match ply with
            | CastleKingSide White
            | CastleQueenSide White
            | Move    (Piece (White, King), _, _)
            | Move    (Piece (White, Rook), (A, R1), _)
            | Capture (Piece (White, Rook), _, _)
            | Capture (Piece (White, King), _, _)
            | Capture (_, _, (A, R1)) -> true
            | _ -> false
        let plyPreventsBlackCastleQueenSide =
            match ply with
            | CastleKingSide Black
            | CastleQueenSide Black
            | Move    (Piece (Black, King), _, _)
            | Move    (Piece (Black, Rook), (A, R8), _)
            | Capture (Piece (Black, Rook), _, _)
            | Capture (Piece (Black, King), _, _)
            | Capture (_, _, (A, R8)) -> true
            | _ -> false

        let hasStructureChanged = Ply.isCapture ply || Ply.shape ply = Pawn
        let nextKingPosition =
            match ply with
            | Move(Piece (color, King), _, target)
            | Capture(Piece (color, King), _, target) -> gameState.kings |> Colored.update color target
            | CastleKingSide White -> gameState.kings |> Colored.update White G1
            | CastleKingSide Black -> gameState.kings |> Colored.update Black G8
            | CastleQueenSide White -> gameState.kings |> Colored.update White C1
            | CastleQueenSide Black -> gameState.kings |> Colored.update Black C8
            | _ -> gameState.kings

        let nextGameStateTemp = {
            kings = nextKingPosition
            playerInTurn = opponent gameState.playerInTurn
            pieces = gameState.pieces |> rawExecutePly ply
            pawnCapturableEnPassant =
                match ply with
                | Move (Piece (White, Pawn), (_, R2), (f, R4)) -> Some (f, R3)
                | Move (Piece (Black, Pawn), (_, R7), (f, R5)) -> Some (f, R6)
                | _ -> None
            castlingRights = {
                white = {
                    canCastleKingSide =
                        gameState.castlingRights.black.canCastleKingSide &&
                        not plyPreventsBlackCastleKingSide
                    canCastleQueenSide =
                        gameState.castlingRights.black.canCastleQueenSide &&
                        not plyPreventsBlackCastleQueenSide
                }
                black = {
                    canCastleKingSide =
                        gameState.castlingRights.white.canCastleKingSide &&
                        not plyPreventsWhiteCastleKingSide
                    canCastleQueenSide =
                        gameState.castlingRights.white.canCastleQueenSide &&
                        not plyPreventsWhiteCastleQueenSide
                }
            }
            plies = gameState.plies
            pliesWithoutPawnOrCapture = if hasStructureChanged then 0 else gameState.pliesWithoutPawnOrCapture + 1
            repeatableStates = if hasStructureChanged then [] else gameState.repeatableState :: gameState.repeatableStates
            numberOfMoves =
                match gameState.playerInTurn with
                | Player White -> gameState.numberOfMoves
                | Player Black -> gameState.numberOfMoves + 1
        }

        let isInCheck = isCheck nextGameStateTemp.playerInTurn nextGameStateTemp

        let newPlyOutput = {
            ply = ply
            restOfPlies = restOfPlies
            checkStatus = isInCheck
            drawOffer = drawOffer
        }
        { nextGameStateTemp with
            plies = newPlyOutput :: nextGameStateTemp.plies
        }

    let internal pieceCapabilities game (piece, sourceCoordinate) =
        let isCheck p g = CheckStatus.toBool <| isCheck p g

        let runsIntoCheck ply =
            nextGameState ply [] NoDrawOffer game
            |> isCheck game.playerInTurn

        let castlingPathAttacked =
            function
            | CastleKingSide White -> isAttackedBy Black game F1
            | CastleKingSide Black -> isAttackedBy White game F8
            | CastleQueenSide White -> isAttackedBy Black game D1
            | CastleQueenSide Black -> isAttackedBy White game D8
            | _ -> false

        pieceCapabilitiesWithoutCheckFilter game piece sourceCoordinate
        |> Seq.where (not << castlingPathAttacked)
        |> Seq.where (not << runsIntoCheck)

module PlayerActionOutcome =
    let state = function
        | Draw (state, _)
        | LostByResignation (state, _)
        | WonByCheckmate (state, _)
        | GameStarted (state, _)
        | PlayerMoved (state, _)
        | DrawOffered (state, _) -> state

    let actions = function
        | Draw _
        | LostByResignation _
        | WonByCheckmate _ -> 
            []
        | GameStarted (_, availableActions)
        | DrawOffered (_, availableActions)
        | PlayerMoved (_, availableActions) ->
            availableActions

module ExecutableAction =
    let action: ExecutableAction -> PlayerAction = fst
    let executeFn: ExecutableAction -> unit -> PlayerActionOutcome = snd

module EngineCore =
    open Player
    open ReachEvaluation
    open ChessState

    // Possible plies for the player in turn.
    let internal playerPlies gameState =
        let getCapabilities (sourceCoordinate, sourcePiece) =
            pieceCapabilities gameState (sourcePiece, sourceCoordinate)
        let (Player playerColor) = gameState.playerInTurn
        let belongsToPlayer _ (Piece (pieceColor, _)) = pieceColor = playerColor
        
        gameState.pieces
        |> Map.filter belongsToPlayer
        |> Map.toSeq
        |> Seq.collect getCapabilities

    
    // given a player & a gameState, it returns a move result for that player.
    let rec private recStart = ()

    // player makes a move
    and internal executePlayerAction (gameState: ChessState) (playerAction: PlayerAction): PlayerActionOutcome =
        match playerAction with
        | MovePiece (ply, restOfPlies, drawOffer) ->
            let newGameState = nextGameState ply restOfPlies drawOffer gameState
            getOutcomeFromNewBoard newGameState
        | Resign ->
            LostByResignation (gameState, (opponent gameState.playerInTurn))
        | AcceptDraw ->
            Draw (gameState, Agreement)

    and internal getOutcomeFromNewBoard (gameState: ChessState) =
        let drawOffer =
            gameState.plies
            |> List.tryHead
            |> Option.map (fun x -> x.drawOffer)
            |> Option.defaultValue NoDrawOffer

        let isDrawOffer =
            match drawOffer with
            | IsDrawOffer -> true
            | NoDrawOffer -> false
            
        if isDrawOffer && gameState.pliesWithoutPawnOrCapture >= 50 then
            Draw (gameState, FiftyMovements)
        elif isDrawOffer &&  gameState.repetitionCount >= 3 then
            Draw (gameState, ThreefoldRepetition)
        elif gameState.pliesWithoutPawnOrCapture > 75 then
            Draw (gameState, SeventyFiveMovements)
        elif gameState.repetitionCount >= 5 then
            Draw (gameState, FivefoldRepetition)
        else
            let playerPlies = playerPlies gameState |> Seq.toList
            let canMove = playerPlies |> (not << List.isEmpty)
            if canMove then
                let actions = getExecutableActions playerPlies gameState drawOffer
                match (gameState.plies |> List.length, drawOffer) with
                | 0, _ -> GameStarted (gameState, actions)
                | _, IsDrawOffer -> DrawOffered (gameState, actions)
                | _, NoDrawOffer -> PlayerMoved (gameState, actions)
            else
                match isCheck gameState.playerInTurn gameState with
                | IsCheck -> WonByCheckmate (gameState, gameState.playerInTurn)
                | NoCheck -> Draw (gameState, Stalemate)

    // Convert possible plies to executable actions, adding Resign and OfferDraw
    and internal getExecutableActions plies gameState drawOffer =
        let movePieces = plies |> Seq.collect (fun ply -> [
            MovePiece (ply, plies, NoDrawOffer)
            MovePiece (ply, plies, IsDrawOffer)
        ])

        movePieces
        |> Seq.append [Resign]
        |> Seq.append (match drawOffer with | IsDrawOffer -> [AcceptDraw] | _ -> [])
        |> Seq.map (makeNextExecutableAction gameState)
        |> Seq.toList

    and internal makeNextExecutableAction gameState playerAction =
        let executeFn() = executePlayerAction gameState playerAction
        playerAction, executeFn

module Setup =
    open Coordinate
    open Piece
    open ReachEvaluation
    open EngineCore

    let initial =
        let initialPieces = Map.ofList [
            A1, WhiteRook
            B1, WhiteKnight
            C1, WhiteBishop
            D1, WhiteQueen
            E1, WhiteKing
            F1, WhiteBishop
            G1, WhiteKnight
            H1, WhiteRook
            A2, WhitePawn
            B2, WhitePawn
            C2, WhitePawn
            D2, WhitePawn
            E2, WhitePawn
            F2, WhitePawn
            G2, WhitePawn
            H2, WhitePawn
            A7, BlackPawn
            B7, BlackPawn
            C7, BlackPawn
            D7, BlackPawn
            E7, BlackPawn
            F7, BlackPawn
            G7, BlackPawn
            H7, BlackPawn
            A8, BlackRook
            B8, BlackKnight
            C8, BlackBishop
            D8, BlackQueen
            E8, BlackKing
            F8, BlackBishop
            G8, BlackKnight
            H8, BlackRook
        ]

        {
            playerInTurn = Player White
            pieces = initialPieces
            castlingRights = {
                white = CastlingRights.bothWays
                black = CastlingRights.bothWays
            }
            pawnCapturableEnPassant = None
            plies = []
            pliesWithoutPawnOrCapture = 0
            repeatableStates = []
            numberOfMoves = 1
            kings = { white = E1; black = E8 }
        }
        
    let check (rep: ChessState) = check rep

    let nullValidate = Ok

    let internal validateStandardChess (gameState: ChessState) =
        let pieceMap (gameState: ChessState) =
            gameState.pieces
            |> Map.toList
            |> List.groupBy snd
            |> List.map (fun (k, pieces) -> (k, List.map (fun (coordinate, _) -> coordinate) pieces))
            |> Map.ofList

        let pieceCoords p = Map.tryFind p >> Option.toResult $"No piece %A{p}"

        let thereIsOnlyPiece = List.length >> (fun x -> x = 1) >> Bool.toResult >> Result.replaceError "There are more than one"

        let (?>>) f1 f2 = f1 >> Option.map f2
        let query f2 = function
        | Some x -> f2 x
        | None -> false
        
        let count p = gameState |> (pieceMap >> Map.tryFind p ?>> List.length >> Option.defaultValue 0)

        let onlyOnePiece p = count p = 1

        let nPiecesOrLess p n = count p <= n
        
        let pieceCoordinateIs piece coordinate =
            pieceMap >> Map.tryFind piece >> query (List.contains coordinate)

        let whiteCastlingKingside = lazy (
            (not gameState.castlingRights.white.canCastleKingSide) ||
            (
            gameState |> (pieceCoordinateIs WhiteKing E1) &&
            gameState |> (pieceCoordinateIs WhiteRook H1))
        )

        let blackCastlingKingside = lazy (
            (not gameState.castlingRights.black.canCastleKingSide) ||
            (
            gameState |> (pieceCoordinateIs BlackKing E8) &&
            gameState |> (pieceCoordinateIs BlackRook H8))
        )


        let numberOfWhitePawns = lazy (count WhitePawn)
        let numberOfWhiteRooks = lazy (count WhiteRook)
        let numberOfWhiteKnights = lazy (count WhiteKnight)
        let numberOfWhiteBishops = lazy (count WhiteBishop)
        let numberOfWhiteQueens = lazy (count WhiteQueen)

        let numberOfBlackPawns = lazy (count BlackPawn)
        let numberOfBlackRooks = lazy (count BlackRook)
        let numberOfBlackKnights = lazy (count BlackKnight)
        let numberOfBlackBishops = lazy (count BlackBishop)
        let numberOfBlackQueens = lazy (count BlackQueen)
        
        let eightWhitePawnsOrLess = lazy(numberOfWhitePawns.Force() <= 8)

        let excessWhitePieces = lazy(
            min (numberOfWhiteRooks.Force() - 2) 0 +
            min (numberOfWhiteKnights.Force() - 2) 0 +
            min (numberOfWhiteBishops.Force() - 2) 0 +
            min (numberOfWhiteQueens.Force() - 1) 0
        )

        let numberOfCapturedWhitePawns = lazy(8 - numberOfWhitePawns.Force())
        
        let moreExcessWhitePiecesThanAllowedByCapturedPawns = lazy(numberOfCapturedWhitePawns.Force() >= excessWhitePieces.Force())

        let noPawnsAtFirstRank = true
        let noPawnsAtLastRank = true

        Ok gameState
        |> Result.filter (onlyOnePiece WhiteKing) (MoreThanOneKing White)
        |> Result.filter (onlyOnePiece BlackKing) (MoreThanOneKing Black)
        |> Result.filter (numberOfWhitePawns.Force() <= 8) (MoreThanEightPawns White)
        |> Result.filter (numberOfBlackPawns.Force() <= 8) (MoreThanEightPawns Black)
        |> Result.filter (moreExcessWhitePiecesThanAllowedByCapturedPawns.Force()) (MoreExcessPiecesThanAbsentPawns White)

    let setupChessPosition gameState validate =
        gameState
        |> validate
        |> Result.map getOutcomeFromNewBoard

    let setupStandardChessPosition gameState =
        setupChessPosition gameState validateStandardChess

    let initialStandardChessPosition () =
        initial |> getOutcomeFromNewBoard

    module ChessState =
        let fromRaw (raw: RawChessState) =
            {
                pieces = raw.pieces
                playerInTurn = raw.playerInTurn
                castlingRights = raw.castlingRights
                pawnCapturableEnPassant = raw.pawnCapturableEnPassant
                pliesWithoutPawnOrCapture = raw.pliesWithoutPawnOrCapture
                plies = []
                numberOfMoves = raw.numberOfMoves
                repeatableStates = []
                kings = {
                    white =
                        raw.pieces
                        |> Map.toSeq
                        |> Seq.filter (fun (_, p) -> match p with | Piece (White, King) -> true | _ -> false)
                        |> Seq.head
                        |> fst
                    black =
                        raw.pieces
                        |> Map.toSeq
                        |> Seq.filter (fun (_, p) -> match p with | Piece (Black, King) -> true | _ -> false)
                        |> Seq.head
                        |> fst
                }
            }
            |> validateStandardChess

        let toRaw (chessState: ChessState) = {
            pieces = chessState.pieces
            playerInTurn = chessState.playerInTurn
            castlingRights = chessState.castlingRights
            pawnCapturableEnPassant = chessState.pawnCapturableEnPassant
            pliesWithoutPawnOrCapture = chessState.pliesWithoutPawnOrCapture
            numberOfMoves = chessState.numberOfMoves
        }
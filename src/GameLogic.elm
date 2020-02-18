module GameLogic exposing (..)

import Board
import Field exposing (Field)
import File exposing (File)
import InputState exposing (InputState(..), SelectionHelper(..), Side(..))
import Piece exposing (Color(..), Piece, PieceType(..))
import Rank exposing (Rank)
import Result.Extra as ResultE


type alias GameState =
    { pieces : List Piece
    , turn : Color
    , canCastle :
        { black : CanCastle
        , white : CanCastle
        }
    }


init : GameState
init =
    { pieces = Board.initPieces
    , turn = White
    , canCastle =
        { black =
            { kingSide = True
            , queenSide = True
            }
        , white =
            { kingSide = True
            , queenSide = True
            }
        }
    }


type alias CanCastle =
    { kingSide : Bool
    , queenSide : Bool
    }


nextTurn : Color -> Color
nextTurn turn =
    case turn of
        White ->
            Black

        Black ->
            White


move : Field -> GameState -> Piece -> Result String ( Piece, Piece, GameState )
move field gameState selected =
    let
        otherPieces =
            List.filter (\piece -> piece /= selected) gameState.pieces

        remainedPieces =
            List.filter (\piece -> piece.field /= field) otherPieces
    in
    if isLegalMove otherPieces field selected then
        let
            target =
                case selected.name of
                    Pawn ->
                        if (Tuple.second >> Rank.toInt) field < 8 then
                            { selected | field = field }

                        else
                            { selected | name = Queen, field = field }

                    _ ->
                        { selected | field = field }

            nextCanCastle =
                let
                    canCastle =
                        gameState.canCastle

                    updateWhite update =
                        { canCastle | white = update canCastle.white }

                    updateBlack update =
                        { canCastle | black = update canCastle.black }
                in
                case ( selected.name, gameState.turn ) of
                    ( King, White ) ->
                        updateWhite (always { kingSide = False, queenSide = False })

                    ( King, Black ) ->
                        updateBlack (always { kingSide = False, queenSide = False })

                    ( Rook, White ) ->
                        if selected.field == ( File.a, Rank.r1 ) then
                            updateWhite (\c -> { c | queenSide = False })

                        else if selected.field == ( File.h, Rank.r1 ) then
                            updateWhite (\c -> { c | kingSide = False })

                        else
                            gameState.canCastle

                    ( Rook, Black ) ->
                        if selected.field == ( File.a, Rank.r8 ) then
                            updateWhite (\c -> { c | queenSide = False })

                        else if selected.field == ( File.h, Rank.r8 ) then
                            updateWhite (\c -> { c | kingSide = False })

                        else
                            gameState.canCastle

                    _ ->
                        gameState.canCastle
        in
        Ok
            ( target
            , selected
            , { gameState
                | pieces = target :: remainedPieces
                , canCastle = nextCanCastle
                , turn = nextTurn gameState.turn
              }
            )

    else
        Err "This move is not possible."


castle : Side -> GameState -> Result String ( Piece, Piece, GameState )
castle side gameState =
    let
        rank =
            case gameState.turn of
                White ->
                    Rank.r1

                Black ->
                    Rank.r8

        forbiddenFields =
            case side of
                QueenSide ->
                    [ ( File.b, rank ), ( File.c, rank ), ( File.d, rank ) ]

                KingSide ->
                    [ ( File.f, rank ), ( File.g, rank ) ]

        allowedToCastle =
            (case ( side, gameState.turn ) of
                ( QueenSide, White ) ->
                    gameState.canCastle.white.queenSide

                ( KingSide, White ) ->
                    gameState.canCastle.white.kingSide

                ( QueenSide, Black ) ->
                    gameState.canCastle.black.queenSide

                ( KingSide, Black ) ->
                    gameState.canCastle.black.kingSide
            )
                && List.all
                    (\piece -> not <| List.member piece.field forbiddenFields)
                    gameState.pieces

        prevKing =
            { name = King, color = gameState.turn, field = ( File.e, rank ) }

        nextKing =
            case side of
                QueenSide ->
                    { name = King, color = gameState.turn, field = ( File.c, rank ) }

                KingSide ->
                    { name = King, color = gameState.turn, field = ( File.g, rank ) }

        prevRook =
            case side of
                QueenSide ->
                    { name = Rook, color = gameState.turn, field = ( File.a, rank ) }

                KingSide ->
                    { name = Rook, color = gameState.turn, field = ( File.h, rank ) }

        nextRook =
            case side of
                QueenSide ->
                    { name = Rook, color = gameState.turn, field = ( File.d, rank ) }

                KingSide ->
                    { name = Rook, color = gameState.turn, field = ( File.f, rank ) }
    in
    if not allowedToCastle then
        Err "You are not allowed to castle."

    else
        let
            canCastle =
                case gameState.turn of
                    White ->
                        { white = { queenSide = False, kingSide = False }
                        , black = gameState.canCastle.black
                        }

                    Black ->
                        { white = gameState.canCastle.white
                        , black = { queenSide = False, kingSide = False }
                        }

            pieces =
                nextKing
                    :: nextRook
                    :: List.filter
                        (\piece ->
                            not <|
                                List.member piece
                                    [ prevKing, prevRook ]
                        )
                        gameState.pieces
        in
        Ok
            ( prevKing
            , nextKing
            , { gameState
                | pieces = pieces
                , turn = nextTurn gameState.turn
                , canCastle = canCastle
              }
            )


evalInputState : InputState -> GameState -> Result String ( Piece, Piece, GameState )
evalInputState inputState gameState =
    case inputState of
        Moved _ _ field ->
            let
                attempts =
                    getSelectedPieces inputState gameState.turn gameState.pieces
                        |> List.map (move field gameState)
                        |> ResultE.partition
            in
            case attempts of
                ( [ onePossibleMove ], _ ) ->
                    Ok onePossibleMove

                ( [], err :: _ ) ->
                    Err err

                _ ->
                    Err "Multiple possible moves. Try to specify which file or rank your piece is on. Ex. Rd5 -> Rad5"

        Castled side ->
            castle side gameState

        _ ->
            Err "Invalid input state"


getSelectedPieces : InputState -> Color -> List Piece -> List Piece
getSelectedPieces inputState turn pieces =
    case inputState of
        Selected name (Just (WithFile file)) ->
            selectByNameAndFile turn name file pieces

        Moved name (Just (WithFile file)) _ ->
            selectByNameAndFile turn name file pieces

        Selected name (Just (WithRank rank)) ->
            selectByNameAndRank turn name rank pieces

        Moved name (Just (WithRank rank)) _ ->
            selectByNameAndRank turn name rank pieces

        Selected name Nothing ->
            selectByName turn name pieces

        Moved name Nothing _ ->
            selectByName turn name pieces

        Castled _ ->
            []

        NotSelected ->
            []


getSelectedFields : InputState -> GameState -> Result String (List Field)
getSelectedFields inputState gameState =
    case inputState of
        Moved _ _ _ ->
            case evalInputState inputState gameState of
                Ok ( target, source, _ ) ->
                    [ target, source ]
                        |> List.map .field
                        |> Ok

                Err err ->
                    Err err

        Castled side ->
            case castle side gameState of
                Ok ( target, source, _ ) ->
                    [ target, source ]
                        |> List.map .field
                        |> Ok

                Err err ->
                    Err err

        _ ->
            getSelectedPieces inputState gameState.turn gameState.pieces
                |> List.map .field
                |> Ok


selectByNameAndFile : Color -> PieceType -> File -> List Piece -> List Piece
selectByNameAndFile turn name file pieces =
    List.filter
        (\piece ->
            (piece.name == name)
                && (piece.color == turn)
                && (Tuple.first piece.field == file)
        )
        pieces


selectByNameAndRank : Color -> PieceType -> Rank -> List Piece -> List Piece
selectByNameAndRank turn name rank pieces =
    List.filter
        (\piece ->
            (piece.name == name)
                && (piece.color == turn)
                && (Tuple.second piece.field == rank)
        )
        pieces


selectByName : Color -> PieceType -> List Piece -> List Piece
selectByName turn name pieces =
    List.filter (\piece -> piece.name == name && piece.color == turn) pieces


isLegalMove : List Piece -> Field -> Piece -> Bool
isLegalMove otherPieces ( nextFile, nextRank ) piece =
    let
        ( prevFile, prevRank ) =
            piece.field

        fileDiff =
            File.toInt nextFile - File.toInt prevFile

        rankDiff =
            Rank.toInt nextRank - Rank.toInt prevRank

        legalMove =
            case piece.name of
                Rook ->
                    fileDiff == 0 && abs rankDiff > 0 || rankDiff == 0 && abs fileDiff > 0

                Bishop ->
                    abs fileDiff == abs rankDiff

                Knight ->
                    abs fileDiff == 2 && abs rankDiff == 1 || abs fileDiff == 1 && abs rankDiff == 2

                Queen ->
                    fileDiff == 0 && abs rankDiff > 0 || rankDiff == 0 && abs fileDiff > 0 || abs fileDiff == abs rankDiff

                King ->
                    (abs fileDiff == 1 && abs rankDiff == 1)
                        || (abs fileDiff == 0 && abs rankDiff == 1)
                        || (abs fileDiff == 1 && abs rankDiff == 0)

                Pawn ->
                    case piece.color of
                        White ->
                            let
                                hasEnemyPiece =
                                    List.any (\otherPiece -> otherPiece.field == ( nextFile, nextRank )) otherPieces
                            in
                            if not hasEnemyPiece then
                                if Rank.toInt prevRank == 2 then
                                    (fileDiff == 0) && (rankDiff == 1 || rankDiff == 2)

                                else
                                    (fileDiff == 0) && (rankDiff == 1)

                            else
                                (abs fileDiff == 1) && (rankDiff == 1)

                        Black ->
                            let
                                hasEnemyPiece =
                                    List.any (\otherPiece -> otherPiece.field == ( nextFile, nextRank )) otherPieces
                            in
                            if not hasEnemyPiece then
                                if Rank.toInt prevRank == 7 then
                                    (fileDiff == 0) && (rankDiff == -1 || rankDiff == -2)

                                else
                                    (fileDiff == 0) && (rankDiff == -1)

                            else
                                (abs fileDiff == 1) && (rankDiff == -1)

        steps =
            let
                fileSteps =
                    if piece.name == Knight then
                        []

                    else if fileDiff > 0 then
                        List.range (File.toInt prevFile + 1) (File.toInt nextFile - 1)

                    else if fileDiff < 0 then
                        List.range (File.toInt nextFile + 1) (File.toInt prevFile - 1) |> List.reverse

                    else
                        []

                rankSteps =
                    if piece.name == Knight then
                        []

                    else if rankDiff > 0 then
                        List.range (Rank.toInt prevRank + 1) (Rank.toInt nextRank - 1)

                    else if rankDiff < 0 then
                        List.range (Rank.toInt nextRank + 1) (Rank.toInt prevRank - 1) |> List.reverse

                    else
                        []

                tupleJoin ( maybeA, maybeB ) =
                    Maybe.map2 Tuple.pair maybeA maybeB
            in
            case ( fileSteps, rankSteps ) of
                ( [], _ ) ->
                    List.map (\rank -> ( Just prevFile, Rank.fromInt rank )) rankSteps
                        |> List.map tupleJoin

                ( _, [] ) ->
                    List.map (\file -> ( File.fromInt file, Just prevRank )) fileSteps
                        |> List.map tupleJoin

                _ ->
                    List.map2 Tuple.pair
                        (List.map File.fromInt fileSteps)
                        (List.map Rank.fromInt rankSteps)
                        |> List.map tupleJoin

        noBlockingPiece =
            List.all
                (\maybeStep ->
                    case maybeStep of
                        Nothing ->
                            False

                        Just step ->
                            List.all (\otherPiece -> otherPiece.field /= step) otherPieces
                )
                steps

        notTakesOwnPiece =
            not <|
                List.any
                    (\otherPiece ->
                        (piece.color == otherPiece.color)
                            && (( nextFile, nextRank ) == otherPiece.field)
                    )
                    otherPieces
    in
    legalMove && noBlockingPiece && notTakesOwnPiece

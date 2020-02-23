module GameLogic exposing (..)

import Data.Board as Board
import Data.Field exposing (Field)
import Data.File as File exposing (File)
import Data.Piece exposing (Color(..), Piece, PieceType(..))
import Data.Rank as Rank exposing (Rank)
import History exposing (History)
import InputState exposing (ExtraInfo(..), InputState(..), SelectionHelper(..), Side(..))
import Result.Extra as ResultE


type alias GameState =
    { pieces : List Piece
    , turn : Color
    , enPassantRight : EnPassantRight
    , castlingRight : CastlingRight
    , history : History
    }


init : GameState
init =
    { pieces = Board.initPieces
    , turn = White
    , enPassantRight =
        { black = Nothing
        , white = Nothing
        }
    , castlingRight =
        { black =
            { kingSide = True
            , queenSide = True
            }
        , white =
            { kingSide = True
            , queenSide = True
            }
        }
    , history = []
    }


type alias EnPassantRight =
    { black : Maybe ( Field, Field )
    , white : Maybe ( Field, Field )
    }


type alias CastlingRight =
    { black :
        { kingSide : Bool
        , queenSide : Bool
        }
    , white :
        { kingSide : Bool
        , queenSide : Bool
        }
    }


nextTurn : Color -> Color
nextTurn turn =
    case turn of
        White ->
            Black

        Black ->
            White


move : Field -> SelectionHelper -> List ExtraInfo -> GameState -> Piece -> Result String ( Piece, Piece, GameState )
move targetField selectionHelper extraInfo gameState selected =
    let
        maybePromotesTo =
            InputState.getPromotesTo extraInfo

        otherPieces =
            List.filter (\piece -> piece /= selected) gameState.pieces

        simpleFilter otherPiece =
            otherPiece.field
                /= targetField

        enPassantFilter otherPiece =
            let
                enPassantRight =
                    case gameState.turn of
                        White ->
                            gameState.enPassantRight.white

                        Black ->
                            gameState.enPassantRight.black
            in
            case enPassantRight of
                Nothing ->
                    True

                Just ( enPassantTarget, enPassantActual ) ->
                    not (enPassantTarget == targetField && enPassantActual == otherPiece.field)

        remainedPieces =
            let
                takenByEnPassant =
                    if selected.name == Pawn then
                        List.filter (not << enPassantFilter) otherPieces

                    else
                        []

                ( remained, taken ) =
                    List.partition
                        (\x -> simpleFilter x && not (List.member x takenByEnPassant))
                        otherPieces
            in
            if List.length taken == 0 then
                if InputState.takes extraInfo || InputState.enPassant extraInfo then
                    Err "There is no piece to take."

                else
                    Ok remained

            else if not (InputState.takes extraInfo) then
                Err "You are taking an enemy piece, try use the x symbol. Ex. Rxe6"

            else if (List.length takenByEnPassant == 0) && InputState.enPassant extraInfo then
                Err "This is not an en passant take."

            else if selected.name == Pawn && selectionHelper == NoSelectionHelper then
                Err "When taking an enemy piece with a pawn, you have to specify the file your pawn is on. Ex. bxd5"

            else
                Ok remained
    in
    if isLegalMove otherPieces gameState.enPassantRight targetField selected then
        let
            target =
                case selected.name of
                    Pawn ->
                        if
                            (selected.color == White && (Tuple.second >> Rank.toInt) targetField == 8)
                                || (selected.color == Black && (Tuple.second >> Rank.toInt) targetField == 1)
                        then
                            case maybePromotesTo of
                                Just promotesTo ->
                                    Ok { selected | name = promotesTo, field = targetField }

                                Nothing ->
                                    Err "Your pawn needs to promote. Use the = symbol to specify a piece type. Ex. e8=Q"

                        else
                            case maybePromotesTo of
                                Nothing ->
                                    Ok { selected | field = targetField }

                                Just _ ->
                                    Err "Only pawns on last rank can promote."

                    _ ->
                        case maybePromotesTo of
                            Nothing ->
                                Ok { selected | field = targetField }

                            Just _ ->
                                Err "Only pawns can promote."

            nextCastlingRight =
                let
                    castlingRight =
                        gameState.castlingRight

                    updateWhite update =
                        { castlingRight | white = update castlingRight.white }

                    updateBlack update =
                        { castlingRight | black = update castlingRight.black }
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
                            gameState.castlingRight

                    ( Rook, Black ) ->
                        if selected.field == ( File.a, Rank.r8 ) then
                            updateWhite (\c -> { c | queenSide = False })

                        else if selected.field == ( File.h, Rank.r8 ) then
                            updateWhite (\c -> { c | kingSide = False })

                        else
                            gameState.castlingRight

                    _ ->
                        gameState.castlingRight

            nextEnPassantRight =
                let
                    enPassantRight =
                        gameState.enPassantRight

                    rankDiff fieldA fieldB =
                        abs
                            ((fieldA |> Tuple.second |> Rank.toInt)
                                - (fieldB |> Tuple.second |> Rank.toInt)
                            )
                in
                case gameState.turn of
                    White ->
                        if
                            (selected.name == Pawn)
                                && (rankDiff selected.field targetField == 2)
                        then
                            { enPassantRight
                                | black =
                                    selected.field
                                        |> Tuple.second
                                        |> (Rank.toInt >> (+) 1 >> Rank.fromInt)
                                        |> Maybe.map
                                            (\between ->
                                                ( ( Tuple.first selected.field, between )
                                                , targetField
                                                )
                                            )
                                , white = Nothing
                            }

                        else
                            { enPassantRight | white = Nothing }

                    Black ->
                        if
                            (selected.name == Pawn)
                                && (rankDiff selected.field targetField == 2)
                        then
                            { enPassantRight
                                | white =
                                    selected.field
                                        |> Tuple.second
                                        |> (Rank.toInt >> (+) -1 >> Rank.fromInt)
                                        |> Maybe.map
                                            (\between ->
                                                ( ( Tuple.first selected.field, between )
                                                , targetField
                                                )
                                            )
                                , black = Nothing
                            }

                        else
                            { enPassantRight | black = Nothing, white = Nothing }
        in
        Result.map2
            (\validTarget validRemainedPieces ->
                ( validTarget
                , selected
                , { gameState
                    | pieces = validTarget :: validRemainedPieces
                    , enPassantRight = nextEnPassantRight
                    , castlingRight = nextCastlingRight
                    , turn = nextTurn gameState.turn
                  }
                )
            )
            target
            remainedPieces

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
                    gameState.castlingRight.white.queenSide

                ( KingSide, White ) ->
                    gameState.castlingRight.white.kingSide

                ( QueenSide, Black ) ->
                    gameState.castlingRight.black.queenSide

                ( KingSide, Black ) ->
                    gameState.castlingRight.black.kingSide
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
        case side of
            QueenSide ->
                Err "You are not allowed to castle queen side."

            KingSide ->
                Err "You are not allowed to castle king side."

    else
        let
            castlingRight =
                case gameState.turn of
                    White ->
                        { white = { queenSide = False, kingSide = False }
                        , black = gameState.castlingRight.black
                        }

                    Black ->
                        { white = gameState.castlingRight.white
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
                , castlingRight = castlingRight
              }
            )


evalInputState : InputState -> GameState -> Result String ( Piece, Piece, GameState )
evalInputState inputState gameState =
    case inputState of
        Moved _ selectionHelper field extraInfo ->
            let
                attempts =
                    getSelectedPieces inputState gameState.turn gameState.pieces
                        |> List.map (move field selectionHelper extraInfo gameState)
                        |> ResultE.partition
            in
            case attempts of
                ( [ ( target, source, nextGameState ) ], _ ) ->
                    Ok
                        ( target
                        , source
                        , { nextGameState
                            | history = inputState :: nextGameState.history
                          }
                        )

                ( [], err :: _ ) ->
                    Err err

                _ ->
                    Err "Multiple possible moves. Try to specify which file or rank your piece is on. Ex. Rd5 -> Rad5"

        Castled side ->
            castle side gameState

        _ ->
            Err "Invalid input state"


getNextGameState : InputState -> GameState -> Result String GameState
getNextGameState inputState gameState =
    Result.map (\( _, _, nextGameState ) -> nextGameState) (evalInputState inputState gameState)


getSelectedPieces : InputState -> Color -> List Piece -> List Piece
getSelectedPieces inputState turn pieces =
    case inputState of
        Selected name (WithFile file) ->
            selectByNameAndFile turn name file pieces

        Moved name (WithFile file) _ _ ->
            selectByNameAndFile turn name file pieces

        Selected name (WithRank rank) ->
            selectByNameAndRank turn name rank pieces

        Moved name (WithRank rank) _ _ ->
            selectByNameAndRank turn name rank pieces

        Selected name (WithField field) ->
            selectByNameAndField turn name field pieces

        Moved name (WithField field) _ _ ->
            selectByNameAndField turn name field pieces

        Selected name NoSelectionHelper ->
            selectByName turn name pieces

        Moved name NoSelectionHelper _ _ ->
            selectByName turn name pieces

        Castled _ ->
            []

        NotSelected ->
            []


getSelectedFields : InputState -> GameState -> Result String (List Field)
getSelectedFields inputState gameState =
    case inputState of
        Moved _ _ _ _ ->
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


selectByNameAndField : Color -> PieceType -> Field -> List Piece -> List Piece
selectByNameAndField turn name field pieces =
    List.filter
        (\piece ->
            (piece.name == name)
                && (piece.color == turn)
                && (piece.field == field)
        )
        pieces


selectByName : Color -> PieceType -> List Piece -> List Piece
selectByName turn name pieces =
    List.filter (\piece -> piece.name == name && piece.color == turn) pieces


isLegalMove : List Piece -> EnPassantRight -> Field -> Piece -> Bool
isLegalMove otherPieces enPassantRight ( nextFile, nextRank ) piece =
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
                                enPassantPiece =
                                    Maybe.map Tuple.first enPassantRight.white

                                hasEnemyPiece =
                                    List.any (\otherPiece -> otherPiece.field == ( nextFile, nextRank )) otherPieces
                                        || (enPassantPiece == Just ( nextFile, nextRank ))
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
                                enPassantPiece =
                                    Maybe.map Tuple.first enPassantRight.black

                                hasEnemyPiece =
                                    List.any (\otherPiece -> otherPiece.field == ( nextFile, nextRank )) otherPieces
                                        || (enPassantPiece == Just ( nextFile, nextRank ))
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


loadHistory : History -> Result String GameState
loadHistory history =
    List.foldr
        (\inputState gameState -> Result.andThen (getNextGameState inputState) gameState)
        (Ok init)
        history

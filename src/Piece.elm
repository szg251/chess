module Piece exposing (Color(..), Field, Piece(..), getColor, getField, move, view)

import Field exposing (fileToX, rankToY)
import Icons.Bishop
import Icons.King
import Icons.Knight
import Icons.Pawn
import Icons.Queen
import Icons.Rook
import Svg exposing (Svg)


type alias Field =
    ( Char, Int )


type Piece
    = Pawn Color Field
    | Rook Color Field
    | Knight Color Field
    | Bishop Color Field
    | Queen Color Field
    | King Color Field


isKnight : Piece -> Bool
isKnight piece =
    case piece of
        Knight _ _ ->
            True

        _ ->
            False


type Color
    = White
    | Black


getColorAndField : Piece -> ( Color, Field )
getColorAndField piece =
    case piece of
        Pawn color field ->
            ( color, field )

        Rook color field ->
            ( color, field )

        Knight color field ->
            ( color, field )

        Bishop color field ->
            ( color, field )

        Queen color field ->
            ( color, field )

        King color field ->
            ( color, field )


getColor : Piece -> Color
getColor =
    getColorAndField >> Tuple.first


getField : Piece -> Field
getField =
    getColorAndField >> Tuple.second


move : List Piece -> Field -> Piece -> Maybe Piece
move otherPieces field piece =
    if isLegalMove otherPieces field piece then
        Just <|
            case piece of
                Pawn color _ ->
                    if Tuple.second field < 8 then
                        Pawn color field

                    else
                        Queen color field

                Rook color _ ->
                    Rook color field

                Knight color _ ->
                    Knight color field

                Bishop color _ ->
                    Bishop color field

                Queen color _ ->
                    Queen color field

                King color _ ->
                    King color field

    else
        Nothing


view : Int -> Piece -> Svg msg
view rotate piece =
    case piece of
        Pawn White ( file, rank ) ->
            Icons.Pawn.white rotate (fileToX file) (rankToY rank)

        Pawn Black ( file, rank ) ->
            Icons.Pawn.black rotate (fileToX file) (rankToY rank)

        King White ( file, rank ) ->
            Icons.King.white rotate (fileToX file) (rankToY rank)

        King Black ( file, rank ) ->
            Icons.King.black rotate (fileToX file) (rankToY rank)

        Queen White ( file, rank ) ->
            Icons.Queen.white rotate (fileToX file) (rankToY rank)

        Queen Black ( file, rank ) ->
            Icons.Queen.black rotate (fileToX file) (rankToY rank)

        Rook White ( file, rank ) ->
            Icons.Rook.white rotate (fileToX file) (rankToY rank)

        Rook Black ( file, rank ) ->
            Icons.Rook.black rotate (fileToX file) (rankToY rank)

        Knight White ( file, rank ) ->
            Icons.Knight.white rotate (fileToX file) (rankToY rank)

        Knight Black ( file, rank ) ->
            Icons.Knight.black rotate (fileToX file) (rankToY rank)

        Bishop White ( file, rank ) ->
            Icons.Bishop.white rotate (fileToX file) (rankToY rank)

        Bishop Black ( file, rank ) ->
            Icons.Bishop.black rotate (fileToX file) (rankToY rank)


isLegalMove : List Piece -> Field -> Piece -> Bool
isLegalMove otherPieces ( nextFile, nextRank ) piece =
    let
        ( prevFile, prevRank ) =
            getField piece

        fileDiff =
            Field.fileToInt nextFile - Field.fileToInt prevFile

        rankDiff =
            nextRank - prevRank

        legalMove =
            case piece of
                Rook _ _ ->
                    fileDiff == 0 && abs rankDiff > 0 || rankDiff == 0 && abs fileDiff > 0

                Bishop _ _ ->
                    abs fileDiff == abs rankDiff

                Knight _ _ ->
                    abs fileDiff == 2 && abs rankDiff == 1 || abs fileDiff == 1 && abs rankDiff == 2

                Queen _ _ ->
                    fileDiff == 0 && abs rankDiff > 0 || rankDiff == 0 && abs fileDiff > 0 || abs fileDiff == abs rankDiff

                King _ _ ->
                    (abs fileDiff == 1 && abs rankDiff == 1)
                        || (abs fileDiff == 0 && abs rankDiff == 1)
                        || (abs fileDiff == 1 && abs rankDiff == 0)

                Pawn White _ ->
                    let
                        hasEnemyPiece =
                            List.any (\otherPiece -> getField otherPiece == ( nextFile, nextRank )) otherPieces
                    in
                    if not hasEnemyPiece then
                        if prevRank == 2 then
                            (fileDiff == 0) && (rankDiff == 1 || rankDiff == 2)

                        else
                            (fileDiff == 0) && (rankDiff == 1)

                    else
                        (abs fileDiff == 1) && (rankDiff == 1)

                Pawn Black _ ->
                    let
                        hasEnemyPiece =
                            List.any (\otherPiece -> getField otherPiece == ( nextFile, nextRank )) otherPieces
                    in
                    if not hasEnemyPiece then
                        if prevRank == 7 then
                            (fileDiff == 0) && (rankDiff == -1 || rankDiff == -2)

                        else
                            (fileDiff == 0) && (rankDiff == -1)

                    else
                        (abs fileDiff == 1) && (rankDiff == -1)

        steps =
            let
                fileSteps =
                    if isKnight piece then
                        []

                    else if fileDiff > 0 then
                        List.range (Field.fileToInt prevFile + 1) (Field.fileToInt nextFile - 1)

                    else if fileDiff < 0 then
                        List.range (Field.fileToInt nextFile + 1) (Field.fileToInt prevFile - 1) |> List.reverse

                    else
                        []

                rankSteps =
                    if isKnight piece then
                        []

                    else if rankDiff > 0 then
                        List.range (prevRank + 1) (nextRank - 1)

                    else if rankDiff < 0 then
                        List.range (nextRank + 1) (prevRank - 1) |> List.reverse

                    else
                        []
            in
            case ( fileSteps, rankSteps ) of
                ( [], _ ) ->
                    List.map (\rank -> ( prevFile, rank )) rankSteps

                ( _, [] ) ->
                    List.map (\file -> ( Field.intToFile file, prevRank )) fileSteps

                _ ->
                    List.map2 Tuple.pair (List.map Field.intToFile fileSteps) rankSteps

        noBlockingPiece =
            List.all
                (\step ->
                    List.all (\otherPiece -> getField otherPiece /= step) otherPieces
                )
                steps

        notTakesOwnPiece =
            not <|
                List.any
                    (\otherPiece ->
                        (getColor piece == getColor otherPiece)
                            && (( nextFile, nextRank ) == getField otherPiece)
                    )
                    otherPieces
    in
    legalMove && noBlockingPiece && notTakesOwnPiece

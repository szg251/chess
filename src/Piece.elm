module Piece exposing (Color(..), Field, Piece(..), getColor, getField, move, view)

import Field exposing (fileToX, rankToY)
import File exposing (File)
import Icons.Bishop
import Icons.King
import Icons.Knight
import Icons.Pawn
import Icons.Queen
import Icons.Rook
import Rank exposing (Rank)
import Svg exposing (Svg)


type alias Field =
    ( File, Rank )


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
                    if (Tuple.second >> Rank.toInt) field < 8 then
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
            File.toInt nextFile - File.toInt prevFile

        rankDiff =
            Rank.toInt nextRank - Rank.toInt prevRank

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
                        if Rank.toInt prevRank == 2 then
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
                        if Rank.toInt prevRank == 7 then
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
                        List.range (File.toInt prevFile + 1) (File.toInt nextFile - 1)

                    else if fileDiff < 0 then
                        List.range (File.toInt nextFile + 1) (File.toInt prevFile - 1) |> List.reverse

                    else
                        []

                rankSteps =
                    if isKnight piece then
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

module History exposing (..)

import Html exposing (Html, li, ol, text)
import InputState exposing (InputState(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , backtrackable
        , chompUntil
        , end
        , loop
        , oneOf
        , spaces
        , succeed
        , symbol
        )


type alias History =
    List InputState


type alias SerializedHistory =
    List String


groupMoves : List String -> List String
groupMoves moves =
    case moves of
        whiteMove :: blackMove :: rest ->
            (whiteMove ++ " " ++ blackMove) :: groupMoves rest

        lastMove ->
            lastMove


view : History -> Html msg
view history =
    List.reverse history
        |> List.map InputState.serialize
        |> groupMoves
        |> (\lines ->
                ol
                    []
                    (List.map (li [] << List.singleton << text) lines)
           )


serialize : History -> String
serialize history =
    List.reverse history
        |> List.map InputState.serialize
        |> groupMoves
        |> List.indexedMap (\index line -> String.fromInt (index + 1) ++ ". " ++ line)
        |> String.join "\n"


parser : Parser History
parser =
    loop [] historyLineParser


historyLineParser : List InputState -> Parser (Step (List InputState) (List InputState))
historyLineParser prevMoves =
    let
        addToList whiteMove blackMove nextStep =
            case blackMove of
                NotSelected ->
                    nextStep (whiteMove :: prevMoves)

                _ ->
                    nextStep (blackMove :: whiteMove :: prevMoves)
    in
    oneOf
        [ succeed addToList
            |. chompUntil "."
            |. symbol "."
            |. spaces
            |= InputState.parser
            |. spaces
            |= InputState.parser
            |= oneOf
                [ Parser.map (always Loop) (symbol "\n")
                , Parser.map (always Done) end
                ]
        ]

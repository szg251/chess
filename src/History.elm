module History exposing (..)

import Html exposing (Html, li, ol, span, strong, text, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import InputState exposing (InputState(..))
import List.Extra as ListE
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


view :
    { history : History
    , alternateHistory : Maybe String
    , selected : Maybe Int
    , replayedMsg : Int -> msg
    , editedMsg : String -> msg
    }
    -> Html msg
view { history, alternateHistory, selected, replayedMsg, editedMsg } =
    case alternateHistory of
        Nothing ->
            let
                viewHistoryLine lineNum steps =
                    li []
                        (List.indexedMap
                            (\index step ->
                                (if selected == Just (lineNum * 2 + index) then
                                    strong

                                 else
                                    span
                                )
                                    [ style "padding" "0 5px"
                                    , style "cursor" "pointer"
                                    , onClick (replayedMsg (lineNum * 2 + index))
                                    ]
                                    [ text step ]
                            )
                            steps
                        )
            in
            ol []
                (List.reverse history
                    |> List.map InputState.serialize
                    |> ListE.greedyGroupsOf 2
                    |> List.indexedMap viewHistoryLine
                )

        Just rawHistory ->
            textarea
                [ onInput editedMsg
                , value rawHistory
                , style "height" "500px"
                , style "width" "300px"
                ]
                []


serialize : History -> String
serialize history =
    List.reverse history
        |> List.map InputState.serialize
        |> ListE.greedyGroupsOf 2
        |> List.indexedMap (\index steps -> String.fromInt (index + 1) ++ ". " ++ String.join " " steps)
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
                [ Parser.map (always Loop) (oneOf [ symbol "\n", symbol " " ])
                , Parser.map (always Done) end
                ]
        ]

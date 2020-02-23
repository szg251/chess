module HistoryTest exposing (..)

import Data.File as File
import Data.Piece exposing (PieceType(..))
import Data.Rank as Rank
import Expect exposing (Expectation)
import History
import InputState exposing (ExtraInfo(..), InputState(..), SelectionHelper(..), Side(..))
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "History parser"
        [ test "parse one liner" <|
            let
                testFile =
                    "1. e4 f5"

                expected =
                    [ Moved Pawn NoSelectionHelper ( File.f, Rank.r5 ) []
                    , Moved Pawn NoSelectionHelper ( File.e, Rank.r4 ) []
                    ]
            in
            \_ -> Expect.equal (Parser.run History.parser testFile) (Ok expected)
        , test "parse half line" <|
            let
                testFile =
                    "1. e4"

                expected =
                    [ Moved Pawn NoSelectionHelper ( File.e, Rank.r4 ) [] ]
            in
            \_ -> Expect.equal (Parser.run History.parser testFile) (Ok expected)
        , test "parse file" <|
            let
                testFile =
                    """1. e4 f5
                       2. exf5 g5
                       3. Qh5#"""

                expected =
                    [ Moved Queen NoSelectionHelper ( File.h, Rank.r5 ) [ Checkmate ]
                    , Moved Pawn NoSelectionHelper ( File.g, Rank.r5 ) []
                    , Moved Pawn (WithFile File.e) ( File.f, Rank.r5 ) [ Takes ]
                    , Moved Pawn NoSelectionHelper ( File.f, Rank.r5 ) []
                    , Moved Pawn NoSelectionHelper ( File.e, Rank.r4 ) []
                    ]
            in
            \_ -> Expect.equal (Parser.run History.parser testFile) (Ok expected)
        ]

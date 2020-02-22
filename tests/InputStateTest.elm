module InputStateTest exposing (..)

import Expect exposing (Expectation)
import File
import InputState exposing (ExtraInfo(..), InputState(..), SelectionHelper(..), Side(..))
import Parser
import Piece exposing (PieceType(..))
import Rank
import Test exposing (..)


testCases : List ( String, String, InputState )
testCases =
    [ ( "Not selected", "", NotSelected )
    , ( "Pawn move", "e4", Moved Pawn NoSelectionHelper ( File.e, Rank.r4 ) [] )
    , ( "Pawn take", "exd5", Moved Pawn (WithFile File.e) ( File.d, Rank.r5 ) [ Takes ] )
    , ( "Pawn takes en passant", "exd6e.p.", Moved Pawn (WithFile File.e) ( File.d, Rank.r6 ) [ Takes, EnPassant ] )
    , ( "Pawn promotes to Queen"
      , "e8=Q"
      , Moved Pawn NoSelectionHelper ( File.e, Rank.r8 ) [ PromotesTo Queen ]
      )
    , ( "Pawn takes and promotes to Queen"
      , "bxc8=Q"
      , Moved Pawn (WithFile File.b) ( File.c, Rank.r8 ) [ Takes, PromotesTo Queen ]
      )
    , ( "Queen selected", "Q", Selected Queen NoSelectionHelper )
    , ( "Knight selected with file helper", "Na", Selected Knight (WithFile File.a) )
    , ( "Bishop selected with rank helper", "B1", Selected Bishop (WithRank Rank.r1) )
    , ( "King moved", "Ke6", Moved King NoSelectionHelper ( File.e, Rank.r6 ) [] )
    , ( "Rook moved with file helper", "Rae6", Moved Rook (WithFile File.a) ( File.e, Rank.r6 ) [] )
    , ( "Rook moved with rank helper", "R1e6", Moved Rook (WithRank Rank.r1) ( File.e, Rank.r6 ) [] )
    , ( "Queen moved with field helper"
      , "Qh4e1"
      , Moved Queen (WithField ( File.h, Rank.r4 )) ( File.e, Rank.r1 ) []
      )
    , ( "Queen takes with field helper"
      , "Qh4xe1"
      , Moved Queen (WithField ( File.h, Rank.r4 )) ( File.e, Rank.r1 ) [ Takes ]
      )
    , ( "Rook takes", "Rxe6", Moved Rook NoSelectionHelper ( File.e, Rank.r6 ) [ Takes ] )
    , ( "Rook takes with file helper", "Raxe6", Moved Rook (WithFile File.a) ( File.e, Rank.r6 ) [ Takes ] )
    , ( "Rook takes with rank helper", "R1xe6", Moved Rook (WithRank Rank.r1) ( File.e, Rank.r6 ) [ Takes ] )
    , ( "Castles queen side", "0-0-0", Castled QueenSide )
    , ( "Castles king side", "0-0", Castled KingSide )
    ]


suite : Test
suite =
    describe "InputState"
        [ describe "parser" <|
            List.map
                (\( name, string, inputState ) ->
                    test name (\_ -> Expect.equal (Parser.run InputState.parser string) (Ok inputState))
                )
                testCases
        , describe "serialize" <|
            List.map
                (\( name, string, inputState ) ->
                    test name (\_ -> Expect.equal (InputState.serialize inputState) string)
                )
                testCases
        ]

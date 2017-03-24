import Test.HUnit
import Lib
--import Data.Map
--import Test.QuickCheck

test_make_onehigh =
  TestCase $ assertEqual
               "make one-high with 10 of clubs"
               (HTOneHigh, [10, 5, 3, 2, 1])
               hand
  where hand = makeHand ((Card Hearts 1), (Card Hearts 5), (Card Clubs 2), (Card Clubs 3), (Card Clubs 10))

test_make_pair =
  TestCase $ assertEqual
               "make pair of ones"
               (HTPair, [1, 10, 8, 5])
               hand
  where hand = makeHand ((Card Hearts 1), (Card Clubs 1), (Card Spades 5), (Card Clubs 8), (Card Clubs 10))

test_make_triple =
  TestCase $ assertEqual
               "make three of a kind with ones"
               (HTTriple, [1,10,8])
               hand
  where hand = makeHand ((Card Hearts 1), (Card Clubs 1), (Card Spades 1), (Card Clubs 8), (Card Clubs 10))

test_make_twopair =
  TestCase $ assertEqual
               "make twopair"
               (HTTwoPair, [2, 1, 10])
               hand
  where hand = makeHand ((Card Hearts 1), (Card Clubs 1), (Card Spades 2), (Card Clubs 2), (Card Clubs 10))

test_make_straight =
  TestCase $ assertEqual
               "make straight"
               (HTStraight, [5])
               hand
  where hand = makeHand ((Card Hearts 1), (Card Clubs 2), (Card Spades 3), (Card Clubs 4), (Card Diamonds 5))

test_make_flush =
  TestCase $ assertEqual
               "make flush"
               (HTFlush, [7,4,3,2,1])
               hand
  where hand = makeHand ((Card Hearts 1), (Card Hearts 2), (Card Hearts 3), (Card Hearts 4), (Card Hearts 7))

test_make_straight_flush =
  TestCase $ assertEqual
               "make straight flush"
               (HTStraightFlush, [6])
               hand
  where hand = makeHand ((Card Hearts 2), (Card Hearts 3), (Card Hearts 4), (Card Hearts 5), (Card Hearts 6))

test_make_full_house =
  TestCase $ assertEqual
               "make full house"
               (HTFullHouse, [3,1])
               hand
  where hand = makeHand ((Card Hearts 1), (Card Clubs 1), (Card Hearts 3), (Card Spades 3), (Card Diamonds 3))

test_full_hous_fight =
  TestCase $ assertBool
               "make full house"
               (badFullHouse < goodFullHouse)
  where badFullHouse = makeHand ((Card Hearts 1), (Card Clubs 1), (Card Hearts 3), (Card Spades 3), (Card Diamonds 3))
        goodFullHouse = makeHand ((Card Hearts 1), (Card Clubs 1), (Card Hearts 8), (Card Spades 8), (Card Diamonds 8))

test_three_of_a_kind_fight_tied_kicker =
  TestCase $ assertBool
               "three of a kind with better second-kicker wins"
               (badTriple < goodTriple)
  where badTriple = makeHand ((Card Hearts 1), (Card Clubs 1), (Card Spades 1), (Card Diamonds 10), (Card Diamonds 3))
        goodTriple = makeHand ((Card Diamonds 1), (Card Clubs 1), (Card Hearts 1), (Card Spades 10), (Card Diamonds 8))

test_fullhouse_beats_pair =
  TestCase $ assertBool
               "full house beats pair"
               (fullHouse > pair)
  where fullHouse = makeHand ((Card Hearts 1), (Card Clubs 1), (Card Spades 1), (Card Diamonds 2), (Card Diamonds 2))
        pair = makeHand ((Card Diamonds 1), (Card Clubs 1), (Card Hearts 3), (Card Spades 5), (Card Diamonds 9))

test_parses_and_compares_hands =
  TestCase $ assertBool
               "pair beats nothing"
               (pair > nothing)
  where pair = parseHand "10H 10D 2C 4C 6C"
        nothing = parseHand "2H 4H 6H 8H 10C"



tests = TestList [
          test_fullhouse_beats_pair,
          test_make_onehigh,
          test_make_pair,
          test_make_triple,
          test_make_twopair,
          test_make_straight,
          test_make_flush,
          test_make_full_house,
          test_make_straight_flush,
          test_full_hous_fight,
          test_three_of_a_kind_fight_tied_kicker,
          test_parses_and_compares_hands
        ]

main :: IO ()
main = do
  runTestTT tests
  return ()

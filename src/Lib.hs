{-# LANGUAGE ScopedTypeVariables #-}
module Lib where
import Data.List
import Data.List.Split
import Data.Maybe
import Safe
import Debug.Trace
import Data.Function

data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Show, Ord)

data Card = Card Suit Integer deriving (Eq, Show)

data HandType =
  HTOneHigh |
  HTPair |
  HTTwoPair |
  HTTriple |
  HTStraight |
  HTFlush |
  HTFullHouse |
  HTQuadruple |
  HTStraightFlush
  deriving (Ord, Eq, Show)

type ComparableHand = (HandType, [Integer])

getSuit (Card suit number) = suit
getNumber (Card suit number) = number

elementsWithLength lists l  = filter (\list -> (length list == l)) lists

largestNumberOccurringTimes numbers times = maximumMay $ numbersOccurringTimes numbers times

numbersOccurringTimes numbers times = let grouped = group $ sort numbers
                                          lists = filter (\list -> (length list == times)) grouped
                                      in map head lists

makeDescend = reverse . sort

orderBySignificance handType targets numbers = do
                    biggestHits :: [Integer] <- traverse (largestNumberOccurringTimes numbers) targets
                    let remainders = makeDescend $ filter (not . (flip elem biggestHits)) numbers
                    return $ (handType, biggestHits ++ remainders)

getPair :: [Integer] -> Maybe ComparableHand
getPair = orderBySignificance HTPair [2]

getTriple :: [Integer] -> Maybe ComparableHand
getTriple = orderBySignificance HTTriple [3]

getQuadruple :: [Integer] -> Maybe ComparableHand
getQuadruple = orderBySignificance HTQuadruple [4]

getTwoPair :: [Integer] -> Maybe ComparableHand
getTwoPair numbers = let twos = sort $ numbersOccurringTimes numbers 2
                     in do
                       bigPair <- atMay twos 1
                       smallPair <- atMay twos 0
                       let remainders = makeDescend $ filter (\x -> not (x `elem` [bigPair, smallPair])) numbers
                       return $ (HTTwoPair, [bigPair, smallPair] ++ remainders)

getFullHouse :: [Integer] -> Maybe ComparableHand
getFullHouse = orderBySignificance HTFullHouse [3, 2]

myshow x = trace (show x) x

isStraight numbers = let lowest = minimum numbers
                         highest = maximum numbers
                         expected = [lowest..highest]
                     in (sort numbers) == expected

getStraight numbers = if isStraight numbers
                        then Just $ (HTStraight, [maximum numbers])
                        else Nothing

numUniqueElements = length . group . sort

isFlush suits = (numUniqueElements suits) == 1

getFlush suits numbers = if isFlush suits
                         then Just $ (HTFlush, makeDescend numbers)
                         else Nothing

getStraightFlush :: [Suit] -> [Integer] -> Maybe ComparableHand
getStraightFlush suits numbers = if (isFlush suits) && (isStraight numbers)
                                 then Just $ (HTStraightFlush, [maximum numbers])
                                 else Nothing

makeHand :: (Card, Card, Card, Card, Card) -> ComparableHand
makeHand (a,b,c,d,e) = let cardList = [a,b,c,d,e]
                           numbers = map getNumber cardList
                           suits = map getSuit cardList
                           descending = reverse $ sort $ numbers
                        in head $ catMaybes [
                          getStraightFlush suits numbers,
                          getQuadruple numbers,
                          getFullHouse numbers,
                          getFlush suits numbers,
                          getStraight numbers,
                          getTwoPair numbers,
                          getTriple numbers,
                          getPair numbers,
                          Just $ (HTOneHigh, descending)
                       ]

newtype CardCollection = CardCollection (Card, Card, Card, Card, Card) deriving (Eq, Show)

instance Ord CardCollection where
  compare = compare `on` (makeHand . getCards)
    where getCards (CardCollection cards) = cards

parseSuit 'H' = Just Hearts
parseSuit 'D' = Just Diamonds
parseSuit 'C' = Just Clubs
parseSuit 'S' = Just Spades
parseSuit _ = Nothing

parseRank "J" = Just 11
parseRank "Q" = Just 12
parseRank "K" = Just 13
parseRank "A" = Just 14
parseRank s = if elem s $ map show [2..10]
              then Just $ read s
              else Nothing

parseCard s = do
                rawRank <- if (length s == 3)
                             then return $ take 2 s
                             else return $ take 1 s
                rawSuit <- lastMay s
                suit <- parseSuit rawSuit
                rank <- parseRank rawRank
                return $ Card suit rank

parseHand s = do
                let splitted = splitOn " " s
                cards <- traverse parseCard splitted
                if (length cards == 5)
                  then
                    let [a,b,c,d,e]=cards
                    in return $ CardCollection (a,b,c,d,e)
                  else Nothing



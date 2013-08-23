module Example.Poker where

import Control.Monad
import Data.List

data Rank
  = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum, Bounded)

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq, Ord, Enum, Bounded)

data Card
  = Card (Suit, Rank)
  | Joker
  deriving (Eq, Show)

data Hand
 = OnePair
 | TwoPair
 deriving (Show, Eq, Ord, Enum, Bounded)

data ParsedHand = ParsedHand {
  hand :: Hand
, cards :: [Card]
} deriving (Show)

-- |
--
-- >>> compareRank Joker Joker
-- EQ
-- >>> compareRank (Card (Spades, King)) Joker
-- LT
-- >>> compareRank Joker (Card (Spades, King))
-- GT
-- >>> compareRank (Card (Hearts, Three)) (Card (Spades, Three))
-- EQ
-- >>> compareRank (Card (Hearts, Two)) (Card (Diamonds, Ace))
-- LT
-- >>> compareRank (Card (Spades, Ace)) (Card (Clubs, Two))
-- GT
compareRank :: Card -> Card -> Ordering
compareRank Joker Joker = EQ
compareRank _     Joker = LT
compareRank Joker _     = GT
compareRank (Card (_, x)) (Card (_, y)) = x `compare` y

-- |
--
-- >>> equalRank Joker Joker
-- True
-- >>> equalRank (Card (Spades, King)) Joker
-- False
-- >>> equalRank Joker (Card (Spades, King))
-- False
-- >>> equalRank (Card (Hearts, Three)) (Card (Spades, Three))
-- True
-- >>> equalRank (Card (Spades, Ace)) (Card (Clubs, Two))
-- False
-- >>> equalRank (Card (Hearts, Two)) (Card (Diamonds, Ace))
-- False
equalRank :: Card -> Card -> Bool
equalRank x y = (compareRank x y) == EQ

-- |
--
-- >>> groupByRank [Joker, Card (Hearts, Ace), Joker, Card (Spades, Ace)]
-- [[Card (Hearts,Ace),Card (Spades,Ace)],[Joker,Joker]]
groupByRank :: [Card] -> [[Card]]
groupByRank [] = []
groupByRank xs = groupBy equalRank $ sortBy compareRank xs

-- |
--
-- >>> kindOf 2 [Card (Spades, Ace), Card (Clubs, Ace), Card (Clubs, Three), Card (Hearts, Five), Card (Diamonds, Two)]
-- [[Card (Spades,Ace),Card (Clubs,Ace)]]
-- >>> kindOf 2 [Card (Spades, Ace), Card (Clubs, Ace), Card (Clubs, Three), Card (Hearts, Five), Card (Diamonds, Three)]
-- [[Card (Clubs,Three),Card (Diamonds,Three)],[Card (Spades,Ace),Card (Clubs,Ace)]]
-- >>> kindOf 3 [Card (Spades, Ace), Card (Clubs, Ace), Card (Clubs, Three), Card (Hearts, Ace), Card (Diamonds, Three)]
-- [[Card (Spades,Ace),Card (Clubs,Ace),Card (Hearts,Ace)]]
kindOf :: Int -> [Card] -> [[Card]]
kindOf n xs = filter (\x -> (length x) >= n) $ groupByRank xs

-- |
--
-- >>> numOfJokers []
-- 0
-- >>> numOfJokers [Card (Spades, Five), Joker]
-- 1
-- >>> numOfJokers [Joker, Card (Spades, Five), Joker]
-- 2
numOfJokers :: [Card] -> Int
numOfJokers xs = length $ filter (== Joker) $ xs

-- |
--
-- >>> (checkOnePair []) :: Maybe ParsedHand
-- Nothing
-- >>> (checkOnePair [Card (Clubs, Two), Card (Clubs, Ace), Card (Hearts, Two)]) :: Maybe ParsedHand
-- Just (ParsedHand {hand = OnePair, cards = [Card (Clubs,Two),Card (Clubs,Ace),Card (Hearts,Two)]})
-- >>> (checkOnePair [Card (Clubs, Two), Card (Clubs, Ace), Card (Hearts, Three)]) :: Maybe ParsedHand
-- Nothing
-- >>> (checkOnePair [Card (Clubs, Two), Card (Clubs, Ace), Joker]) :: Maybe ParsedHand
-- Just (ParsedHand {hand = OnePair, cards = [Card (Clubs,Two),Card (Clubs,Ace),Joker]})
checkOnePair :: (MonadPlus m) => [Card] -> m ParsedHand
checkOnePair xs
  | onePair || oneJoker = return (ParsedHand OnePair xs)
  | otherwise = mzero
  where
    np = length $ kindOf 2 xs
    nj = numOfJokers xs
    nc = length $ xs
    onePair  = np >= 1
    oneJoker = nj >= 1 && nc >= 2

---- |
----
---- >>> (checkTwoPair [Card (Clubs, Two), Card (Clubs, Ace), Card (Hearts, Two)]) :: [ParsedHand]
---- []
---- >>> map (hand) $ (checkTwoPair [Card (Clubs, Two), Card (Hearts, Two), Card (Spades, Three), Card (Hearts, Three)]) :: [ParsedHand]
---- [TwoPair]
---- >>> map (hand) $ (checkTwoPair [Card (Clubs, Two), Card (Hearts, Two), Card (Spades, Three), Joker]) :: [ParsedHand]
---- [TwoPair]
---- >>> map (hand) $ (checkTwoPair [Card (Clubs, Two), Joker, Card (Spades, Three), Joker]) :: [ParsedHand]
---- [TwoPair]
---- >>> map (hand) $ (checkTwoPair [Joker, Joker, Card (Spades, Three), Joker]) :: [ParsedHand]
---- [TwoPair]
checkTwoPair :: (MonadPlus m) => [Card] -> m ParsedHand
checkTwoPair xs
  | twoPair || twoJoker || oneJoker = return (ParsedHand TwoPair xs)
  | otherwise = mzero
  where
    np = length $ kindOf 2 xs
    nj = numOfJokers xs
    nc = length $ xs
    twoPair  = np >= 2
    twoJoker = nj >= 2 && nc >= 4
    oneJoker = np == 1 && nj == 1 && nc >= 4

-- |
--
-- >>> (checkHands [Card (Clubs, Two), Card (Clubs, Ace), Card (Hearts, Four)]) :: Maybe ParsedHand
-- Nothing
-- >>> map (hand) $ ((checkHands [Card (Clubs, Two), Card (Clubs, Ace), Card (Hearts, Ace)]) :: [ParsedHand])
-- [OnePair]
-- >>> ((checkHands [Card (Clubs, Two), Card (Clubs, Ace), Card (Hearts, Ace), Joker]) >>= \x -> return (hand x)) :: Maybe Hand
-- Just TwoPair
-- >>> map (hand) $ ((checkHands [Card (Clubs, Two), Card (Clubs, Ace), Card (Hearts, Ace), Joker]) :: [ParsedHand])
-- [TwoPair,OnePair]
checkHands :: (MonadPlus m) => [Card] -> m ParsedHand
checkHands xs =
  (checkTwoPair xs) `mplus`
  (checkOnePair xs)

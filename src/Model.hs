{-# LANGUAGE FlexibleContexts, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module Model (
    Color (..)
  , Number (..)
  , Shape (..)
  , Fill (..)
  , Card (..)
  , Game (..)
  , cardDeck
  , isSolution
  , initGame
  , removeCards
  , solutionCards
  , solutions
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Control.Monad.Random
import Control.Monad.State
import Data.List
import System.Random.Shuffle

data Color = Red | Green | Blue deriving (Enum, Eq, NFData, Generic)
data Number = One | Two | Three deriving (Enum, Eq, NFData, Generic)
data Shape = Circle | Diamond | Box deriving (Enum, Eq, NFData, Generic)
data Fill = Empty | Half | Full deriving (Enum, Eq, NFData, Generic)

data Card = Card {
    cardColor :: Color
  , cardNumber :: Number
  , cardShape :: Shape
  , cardFill :: Fill
  } deriving (Eq, Typeable, NFData, Generic)

data Game = Game {
    gameAll :: [Card]
  , gameDealt :: [Card]
  , gameConsumed :: [Card]
  } deriving (Generic, NFData)

instance Show Color where
  show Red = "R"
  show Green = "G"
  show Blue = "B"

instance Show Number where
  show One = "1"
  show Two = "2"
  show Three = "3"

instance Show Shape where
  show Circle = "o"
  show Diamond = "d"
  show Box = "b"

instance Show Fill where
  show Empty = "E"
  show Half = "C"
  show Full = "O"

instance Show Card where
  show (Card c n s f) = "(" ++ show c ++ " " ++ show n ++ " " ++ show s ++ " " ++ show f ++ ")"

instance Bounded Card where
  minBound = (toEnum cardMinBound)
  maxBound = (toEnum cardMaxBound)

instance Show Game where
  show (Game a (a1:a2:a3:b1:b2:b3:c1:c2:c3:d1:d2:d3:[]) c) =
    unlines [show a,
    unlines (map unwords [
        map show [a1, a2, a3]
      , map show [b1, b2, b3]
      , map show [c1, c2, c3]
      , map show [d1, d2, d3]])
    , show c
    , "all: " ++ show (length a)
    , "consumed: " ++ show (length c)]
  show _ = "Error: Malformed card game"

instance Enum Card where
  toEnum i = if i >= cardMinBound && i <= cardMaxBound
                then cardDeck !! i
                else error $ "toEnum{Game}: tag (" ++ show i ++ ") is outside of enumeration's range (" ++ show cardMinBound ++ ", " ++ show cardMaxBound ++ ")"
  fromEnum c = case findIndex (==c) cardDeck of
                 Nothing -> error $ "fromEnum{Game}: Card does not exist: " ++ show c
                 (Just i) -> i

cardDeck = [Card c n s f
              | c <- [Red .. Blue],
                n <- [One .. Three],
                s <- [Circle .. Box],
                f <- [Empty .. Full]]

cardMinBound = 0
cardMaxBound = (length cardDeck) - 1

initGame :: (MonadRandom m) => m Game
initGame = do
  dealt <- getDealt
  a <- shuffleM cardDeck
  return $ Game (filter (\x -> x `notElem` dealt) a) dealt []
     where getDealt :: (MonadRandom m) => m [Card]
           getDealt = do
             d <- (map toEnum) `fmap` randomList 12
             if null $ filter isSolution (allCombinations d)
                then getDealt
                else return d

removeCards :: [Card] -> Game -> Game
removeCards cs (Game a d r) = Game newAll newDealt newUsed
  where newAll = filter (\x -> x `notElem` newDealt) a
        newUsed = r ++ cs
        newDealt = (filter (\x -> x `notElem` cs) d) ++ (take 3 (solutionCards d a))

isSolution :: (Card, Card, Card) -> Bool
isSolution ((Card c1 n1 s1 f1), (Card c2 n2 s2 f2), (Card c3 n3 s3 f3)) =
    m (fromEnum c1) (fromEnum c2) (fromEnum c3)
  && m (fromEnum n1) (fromEnum n2) (fromEnum n3)
  && m (fromEnum s1) (fromEnum s2) (fromEnum s3)
  && m (fromEnum f1) (fromEnum f2) (fromEnum f3)
    where m x1 x2 x3 = ((x1 == x2) && (x2 == x3))
                     || ((x1 /= x2) && (x2 /= x3) && (x1 /= x3))

allCombinations :: Enum a => [a] -> [(a, a, a)]
allCombinations [] = []
allCombinations (x:[]) = []
allCombinations xs = [(xs !! x, xs !! y, xs !! z)
  | x <- [0 .. length xs - 3 ]
  , y <- [succ x .. length xs - 2]
  , z <- [succ y .. length xs - 1]]

solutions :: [Card] -> [(Card, Card, Card)]
solutions cs = filter isSolution (allCombinations cs)

solutionCards :: [Card] -> [Card] -> [Card]
solutionCards d a = foldr (\c as ->
                          if null $ solutions (c:d)
                             then as
                             else c:as
                          ) [] a

newNumber :: (MonadRandom m, MonadState [Int] m) => m ()
newNumber = do
  d <- get
  n <- getRandomR (cardMinBound, cardMaxBound)
  if n `elem` d
     then newNumber
     else put (n:d)

randomList :: MonadRandom m => Int -> m [Int]
randomList n = execStateT (sequence $ replicate n newNumber) []

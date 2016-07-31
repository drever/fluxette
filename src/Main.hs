{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import React.Flux

import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Except
import Data.List
import qualified Data.Text as T

import Data.Typeable (Typeable)

data Color = Red | Green | Blue deriving (Enum, Eq)
instance Show Color where
  show Red = "R"
  show Green = "G"
  show Blue = "B"

data Number = One | Two | Three deriving (Enum, Eq)
instance Show Number where
  show One = "1"
  show Two = "2"
  show Three = "3"

data Shape = Circle | Diamond | Box deriving (Enum, Eq)
instance Show Shape where
  show Circle = "o"
  show Diamond = "d"
  show Box = "b"

data Fill = Empty | Half | Full deriving (Enum, Eq)
instance Show Fill where
  show Empty = "C"
  show Half = "E"
  show Full = "O"

data Card = Card {
    cardColor :: Color
  , cardNumber :: Number
  , cardShape :: Shape
  , cardFill :: Fill
  } deriving (Eq, Typeable)

instance Show Card where
  show (Card c n s f) = "(" ++ show c ++ " " ++ show n ++ " " ++ show s ++ " " ++ show f ++ ")"

instance Bounded Card where
  minBound = (toEnum cardMinBound)
  maxBound = (toEnum cardMaxBound)

cardMinBound = 0
cardMaxBound = 80

data Game = Game {
    gameAll :: [Card]
  , gameDealt :: [Card]
  , gameConsumed :: [Card]
  }

instance Show Game where
  show (Game a (a1:a2:a3:a4:b1:b2:b3:b4:c1:c2:c3:c4:d1:d2:d3:d4:[]) c) =
    show a1 ++ " " ++ show a2 ++ " " ++ show a3 ++ " " ++ show a4 ++ "\n" ++
    show b1 ++ " " ++ show b2 ++ " " ++ show b3 ++ " " ++ show b4 ++ "\n" ++
    show c1 ++ " " ++ show c2 ++ " " ++ show c3 ++ " " ++ show c4 ++ "\n" ++
    show d1 ++ " " ++ show d2 ++ " " ++ show d3 ++ " " ++ show d4 ++ "\n"

instance Enum Card where
  toEnum i = if i >= 0 && i < 81
                then allCards !! i
                else error $ "toEnum{Game}: tag (" ++ show i ++ ") is outside of enumeration's range (" ++ show cardMinBound ++ ", " ++ show cardMaxBound ++ ")"
  fromEnum c = case findIndex (==c) allCards of
                 Nothing -> error $ "fromEnum{Game}: Card does not exist: " ++ show c
                 (Just i) -> i

allCards = [Card c n s f
              | c <- [Red .. Blue],
                n <- [One .. Three],
                s <- [Circle .. Box],
                f <- [Empty .. Full]]

initGame :: (MonadRandom m) => m Game
initGame = do
  dealt <- getDealt
  return $ Game allCards dealt []
     where getDealt :: (MonadRandom m) => m [Card]
           getDealt = do
             d <- (map toEnum) `fmap` randomList 16
             if null $ filter isSolution (allCombinations d)
                then getDealt
                else return d

-- logic

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
allCombinations xs = [(xs !! x, xs !! y, xs !! z) | x <- [0 .. length xs - 3 ], y <- [succ x..length xs - 2], z <- [succ y.. length xs - 1]]
solutions xs = filter isSolution (allCombinations xs)

-- test data

testsolutions :: [(Card, Card, Card)]
testsolutions = [ (Card Red One Diamond Full, Card Green Two Diamond Full, Card Blue Three Diamond Full)
                , (Card Blue One Diamond Half, Card Blue One Circle Empty, Card Blue One Box Full)]

testNonSolutions :: [(Card, Card, Card)]
testNonSolutions = [ (Card Red One Diamond Full, Card Red One Box Empty, Card Green Two Box Empty),
                     (Card Green One Diamond Full, Card Green One Diamond Empty, Card Green Two Diamond Empty) ]


-- Util

newNumber :: (MonadRandom m, MonadState [Int] m) => m ()
newNumber = do
  d <- get
  n <- getRandomR (0, 80)
  if n `elem` d
     then newNumber
     else put (n:d)

randomList :: MonadRandom m => Int -> m [Int]
randomList n = execStateT (sequence $ replicate n newNumber) []

-- react-flux

fillAlpha :: Fill -> String
fillAlpha Full = "1"
fillAlpha Half = "0.5"
fillAlpha Empty = "0"

jsColor :: Color -> String
jsColor Red = "rgb(255, 0, 0)"
jsColor Green = "rgb(0, 255, 0)"
jsColor Blue = "rgb(0, 0, 255)"

cardsApp :: ReactView Game
cardsApp = defineControllerView "cards app" cardsStore $ \cardState g ->
  div_ $ do
    h1_ "Welcome to cards game. This is cards game."
    svg_ [ "width" $= "1000"
         , "height" $= "1000" ] (mapM_ card_ $ (zip [1..] (gameDealt g)))

card :: ReactView (Int, Card)
card = defineView "card" $ \(i, c) ->
  case c of
    Card c n Diamond f -> diamond_ i c f n
    Card c n Box f -> box_ i c f n
    Card c n Circle f -> Main.circle_ i c f n

card_ :: (Int, Card) -> ReactElementM eventHandler ()
card_ !c = viewWithIKey card (fst c) c mempty

diamond :: Int -> Color -> Fill -> Number -> ReactView ()
diamond i c f n = defineView "diamond" $ \() ->
    g_ (cardProps i Diamond c f n)
      (boundingBox i >>
        (case n of
          One -> diam cx 45
          Two -> diam cx 35 >> diam cx 60
          Three -> diam cx 20 >> diam cx 45 >> diam cx 70))
            where cx = 26
                  diam :: Int -> Int -> ReactElementM [SomeStoreAction] ()
                  diam x y = path_ ["d" @= unwords [
                                           "M", show (x + 0), show (y + 10),
                                           "L", show (x + 10), show (y + 20),
                                           "L", show (x + 20), show (y + 10),
                                           "L", show (x + 10), show (y + 0), "z"]
                                    , "stroke" @= jsColor c
                                    , "fill-opacity" @= fillAlpha f
                                    , "fill" @= jsColor c] ""

diamond_ :: Int -> Color -> Fill -> Number -> ReactElementM eventHandler ()
diamond_ i c f n = view (diamond i c f n) () mempty

circle :: Int -> Color -> Fill -> Number -> ReactView ()
circle i c f n = defineView "circle" $ \() ->
    g_ (cardProps i Circle c f n)
      (boundingBox i >>
        (case n of
           One -> circ 35 50
           Two -> circ 35 40 >> circ 35 65
           Three -> circ 35 25 >> circ 35 50 >> circ 35 75))
          where circ :: Int -> Int -> ReactElementM [SomeStoreAction] ()
                circ x y = React.Flux.circle_ [ "r" $= "10"
                                              , "cx" @= (show x)
                                              , "cy" @= (show y)
                                              , "fill-opacity" @= fillAlpha f
                                              , "fill" @= jsColor c
                                              , "stroke" @= jsColor c
                                              , "key" @= ("circle" ++ show i ++ show x ++ show y)] ""

cardProps i s c f n = [
         "className" @= ("shape" ++ show s ++ " color" ++ show c ++ " fill" ++ show f ++ " number" ++ show n)
       , "key" @= (show i ++ show s)
       , "transform" @= t]
         where x = show $ i `mod` (4 :: Int) * 70
               y = show $ (floor (toRational i / 4.0)) `mod` (4 :: Int) * 100
               t = "translate(" ++ x ++ "," ++ y ++ ")"

circle_ :: Int -> Color -> Fill -> Number -> ReactElementM eventHandler ()
circle_ i c f n = view (circle i c f n) () mempty

box :: Int -> Color -> Fill -> Number -> ReactView ()
box i c f n = defineView "box" $ \() ->
    g_ (cardProps i Box c f n)
      (boundingBox i >>
        (case n of
         One -> rect cx 45
         Two -> rect cx 35 >> rect cx 60
         Three -> rect cx 20 >> rect cx 45 >> rect cx 70))
            where rect :: Int -> Int -> ReactElementM [SomeStoreAction] ()
                  rect x y = rect_ [ "x" @= (show x)
                                   , "y" @= (show y)
                                   , "width" $= "20"
                                   , "fill-opacity" @= fillAlpha f
                                   , "fill" @= jsColor c
                                   , "stroke" @= jsColor c
                                   , "height" $= "20" ] "rect"
                  cx = 26

boundingBox :: Int -> ReactElementM [SomeStoreAction] ()
boundingBox k = rect_ [ "key" @= k
                      , "stroke" $= "black"
                      , "fill" $= "white"
                      , "stroke-width" $= "2"
                      , "x" $= "5"
                      , "y" $= "5"
                      , "width" $= "60"
                      , "height" $= "90"] ""

box_ :: Int -> Color -> Fill -> Number -> ReactElementM eventHandler ()
box_ i c f n = view (box i c f n) () mempty


-- game :: ReactView Game
-- game = defineView "game" $ \(Game _ d _) ->
    -- mapM_ (div_ >>= card) d

cardsStore :: ReactStore Game
cardsStore = do
  let g = runRand initGame (mkStdGen 0)
  mkStore $ fst g

data GameAction = GameCreate

instance StoreData Game where
  type StoreAction Game = GameAction
  transform action (Game a d c) = do
    newGame <- case action of
                 GameCreate -> initGame
    return newGame

-- main
main :: IO ()
main = do
  g <- initGame
  putStrLn $ show g
  reactRender "flux-test" cardsApp g
  -- x <- reactRenderToString True cardsApp g
  -- putStrLn (T.unpack x)



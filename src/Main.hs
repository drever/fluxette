{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import React.Flux

import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Except
import Data.List
import qualified Data.Text as T

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq

data Color = Red | Green | Blue deriving (Enum, Eq, NFData, Generic)
instance Show Color where
  show Red = "R"
  show Green = "G"
  show Blue = "B"

data Number = One | Two | Three deriving (Enum, Eq, NFData, Generic)
instance Show Number where
  show One = "1"
  show Two = "2"
  show Three = "3"

data Shape = Circle | Diamond | Box deriving (Enum, Eq, NFData, Generic)
instance Show Shape where
  show Circle = "o"
  show Diamond = "d"
  show Box = "b"

data Fill = Empty | Half | Full deriving (Enum, Eq, NFData, Generic)
instance Show Fill where
  show Empty = "C"
  show Half = "E"
  show Full = "O"

data Card = Card {
    cardColor :: Color
  , cardNumber :: Number
  , cardShape :: Shape
  , cardFill :: Fill
  } deriving (Eq, Typeable, NFData, Generic)

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
  } deriving (Generic, NFData)

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
  return $ Game (filter (\x -> x `notElem` dealt) allCards) dealt []
     where getDealt :: (MonadRandom m) => m [Card]
           getDealt = do
             d <- (map toEnum) `fmap` randomList 12
             if null $ filter isSolution (allCombinations d)
                then getDealt
                else return d

removeCards :: [Card] -> Game -> Game
removeCards cs (Game a d r) = Game newAll newDealt newUsed
  where newAll = filter (\x -> x `notElem` newDealt) a
        newDealt = (filter (\x -> x `notElem` cs) d) ++ (take 3 a)
        newUsed = r ++ cs

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
  n <- getRandomR (cardMinBound, cardMaxBound)
  if n `elem` d
     then newNumber
     else put (n:d)

randomList :: MonadRandom m => Int -> m [Int]
randomList n = execStateT (sequence $ replicate n newNumber) []

-- react-flux

fillAlpha :: Fill -> String
fillAlpha Full = "1"
fillAlpha Half = "0.2"
fillAlpha Empty = "0"

jsColor :: Color -> String
jsColor Red = "rgb(255, 0, 0)"
jsColor Green = "rgb(0, 255, 0)"
jsColor Blue = "rgb(0, 0, 255)"

cardsApp :: ReactView ()
cardsApp = defineControllerView "cards app" cardsStore $ \cardState () ->
  div_ $ do
    h1_ "Welcome to cards game. This is cards game."
    svg_ [ "width" $= "1000"
         , "height" $= "1000" ] (mapM_ card_ $ (zip [1..] (map (\c -> (c `elem` (gameStateSelection cardState), c)) (gameDealt $ gameStateGame cardState))))

card :: ReactView (Int, (Bool, Card))
card = defineView "card" $ \(i, (s, c)) ->
  case c of
    Card c n Diamond f -> diamond_ s i c f n
    Card c n Box f -> box_ s i c f n
    Card c n Circle f -> Main.circle_ s i c f n

card_ :: (Int, (Bool, Card)) -> ReactElementM eventHandler ()
card_ !a@(i, (s, c)) = viewWithIKey card i a mempty

diamond :: Bool -> Int -> Color -> Fill -> Number -> ReactView ()
diamond s i c f n = defineView "diamond" $ \() ->
    g_ ((onClick $ \_ _ -> dispatchGame (GameSelect (Card c n Diamond f))):cardProps i Diamond c f n)
      (boundingBox s (show i ++ show c ++ show f ++ show n) >>
        (case n of
          One -> diam cx 40
          Two -> diam cx 30 >> diam cx 55
          Three -> diam cx 15 >> diam cx 40 >> diam cx 65))
            where cx = 26
                  diam :: Int -> Int -> ReactElementM [SomeStoreAction] ()
                  diam x y = path_ ["d" @= unwords [
                                           "M", show (x + 0), show (y + 10),
                                           "L", show (x + 10), show (y + 20),
                                           "L", show (x + 20), show (y + 10),
                                           "L", show (x + 10), show (y + 0), "z"]
                                    , "stroke" @= jsColor c
                                    , "fillOpacity" @= fillAlpha f
                                    , "fill" @= jsColor c] ""

diamond_ :: Bool -> Int -> Color -> Fill -> Number -> ReactElementM eventHandler ()
diamond_ s i c f n = view (diamond s i c f n) () mempty

circle :: Bool -> Int -> Color -> Fill -> Number -> ReactView ()
circle s i c f n = defineView "circle" $ \() ->
    g_ ((onClick $ \_ _ -> dispatchGame (GameSelect (Card c n Circle f))):cardProps i Circle c f n)
      (boundingBox s (show i ++ show c ++ show f ++ show n) >>
        (case n of
           One -> circ 35 50
           Two -> circ 35 40 >> circ 35 65
           Three -> circ 35 25 >> circ 35 50 >> circ 35 75))
          where circ :: Int -> Int -> ReactElementM [SomeStoreAction] ()
                circ x y = React.Flux.circle_ [ "r" $= "10"
                                              , "cx" @= (show x)
                                              , "cy" @= (show y)
                                              , "fillOpacity" @= fillAlpha f
                                              , "fill" @= jsColor c
                                              , "stroke" @= jsColor c
                                              , "key" @= ("circle" ++ show i ++ show x ++ show y)] ""

cardProps i s c f n = [
         "className" @= ("shape" ++ show s ++ " color" ++ show c ++ " fill" ++ show f ++ " number" ++ show n)
       , "key" @= (show i ++ show s ++ show c ++ show c ++ show f ++ show n)
       , "transform" @= t]
         where x = show $ i `mod` (3 :: Int) * 70
               y = show $ (floor (toRational i / 3.0)) `mod` (4 :: Int) * 100
               t = "translate(" ++ x ++ "," ++ y ++ ")"

circle_ :: Bool -> Int -> Color -> Fill -> Number -> ReactElementM eventHandler ()
circle_ s i c f n = view (circle s i c f n) () mempty

box :: Bool -> Int -> Color -> Fill -> Number -> ReactView ()
box s i c f n = defineView "box" $ \() ->
    g_ ((onClick $ \_ _ -> dispatchGame (GameSelect (Card c n Box f))):cardProps i Box c f n)
      (boundingBox s (show i ++ show c ++ show f ++ show n) >>
        (case n of
         One -> rect cx 40
         Two -> rect cx 30 >> rect cx 55
         Three -> rect cx 15 >> rect cx 40 >> rect cx 65))
            where rect :: Int -> Int -> ReactElementM [SomeStoreAction] ()
                  rect x y = rect_ [ "x" @= (show x)
                                   , "y" @= (show y)
                                   , "width" $= "20"
                                   , "fillOpacity" @= fillAlpha f
                                   , "fill" @= jsColor c
                                   , "stroke" @= jsColor c
                                   , "height" $= "20" ] "rect"
                  cx = 26

boundingBox :: Bool -> String -> ReactElementM [SomeStoreAction] ()
boundingBox s k = rect_ [ "key" @= k
                        , "stroke" $= "black"
                        , "fill" $= if not s then "white" else "rgb(200, 250, 250)"
                        , "strokeWidth" $= "2"
                        , "x" $= "5"
                        , "y" $= "5"
                        , "width" $= "60"
                        , "height" $= "90"] ""

box_ :: Bool -> Int -> Color -> Fill -> Number -> ReactElementM eventHandler ()
box_ s i c f n = view (box s i c f n) () mempty

cardsStore :: ReactStore GameState
cardsStore = do
  let g = runRand initGame (mkStdGen 0)
  mkStore (GameState (fst g) [])

data GameAction =
    GameCreate
  | GameSelect Card deriving (Typeable, Generic, NFData)

data GameState = GameState {
    gameStateGame :: Game
  , gameStateSelection :: [Card]
  } deriving (Show, Typeable, Generic, NFData)

instance StoreData GameState where
  type StoreAction GameState = GameAction
  transform action (GameState g s) = do
    newGameState <- case action of
                      GameCreate -> do
                                      ng <- initGame
                                      putStrLn $ "GameCreate with the following state:"
                                      putStrLn $ show g
                                      putStrLn $ show s
                                      return $ GameState ng []
                      (GameSelect c) -> do putStrLn $ "selected card: " ++ show c
                                           if c `elem` s
                                             then return $ GameState g (filter (/=c) s)
                                             else (if length s == 2
                                                       then if isSolution (c, s !! 0, s !! 1)
                                                               then do
                                                                 let ng = (GameState (removeCards (c:s) g) [])
                                                                 putStrLn "GameSelect, new state:"
                                                                 putStrLn (show ng)
                                                                 return ng
                                                               -- TODO Define game logic correctly
                                                               else do
                                                                 putStrLn "No solution, deselect all"
                                                                 return (GameState g [])
                                                       else return (GameState g (c:s)))
    putStrLn $ show newGameState
    return newGameState

-- dispatcher
dispatchGame :: GameAction -> [SomeStoreAction]
dispatchGame a = [SomeStoreAction cardsStore a]

-- main
main :: IO ()
main = do
  reactRender "flux-cards" cardsApp ()
  -- x <- reactRenderToString True cardsApp g
  -- putStrLn (T.unpack x)



{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module Store where

import React.Flux
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Control.Monad.Random

import Model

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

cardsStore :: ReactStore GameState
cardsStore = do
  let g = runRand initGame (mkStdGen 0)
  mkStore (GameState (fst g) [])



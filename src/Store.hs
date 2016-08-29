{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase #-}
module Store where

import React.Flux
import React.Flux.Ajax
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Control.Monad.Random
import Data.Aeson

import Model

data GameAction =
    GameCreate Game
  | GameSelect Card
  | GameGetCurrentGame
  | GameError String
  deriving (Typeable, Generic, NFData)

data GameState = GameState {
    gameStateGame :: Game
  , gameStateSelection :: [Card]
  } deriving (Show, Typeable, Generic, NFData)

instance FromJSON GameState
instance ToJSON GameState

instance StoreData GameState where
  type StoreAction GameState = GameAction
  transform action gs@(GameState g s) = do
    newGameState <- case action of
                      GameCreate g -> gameSetAction g
                      GameError s -> gameError s
                      (GameSelect c) -> gameSelectAction c
                      GameGetCurrentGame -> getCurrentGame gs
    putStrLn $ show newGameState
    return newGameState
      where gameSetAction g = do
                                  putStrLn $ "Set the game with state: "
                                  putStrLn $ show g
                                  return $ GameState g []
            gameError s = do
              putStrLn "Error while performing ajax call"
              putStrLn s
              g <- initGame
              return (GameState g [])
            gameSelectAction c = do putStrLn $ "selected card: " ++ show c
                                    if c `elem` s
                                     then return $ GameState g (filter (/=c) s)
                                     else (if length s == 2
                                             then if isSolution (c, s !! 0, s !! 1)
                                                  then do
                                                        let ng = (GameState (removeCards (c:s) g) [])
                                                        putStrLn "GameSelect, new state:"
                                                        putStrLn (show ng)
                                                        return ng
                                                  else do
                                                        putStrLn "No solution, deselect all"
                                                        return (GameState g [])
                                             else return (GameState g (c:s)))
            getCurrentGame :: GameState -> IO GameState
            getCurrentGame g = do putStrLn $ "Get the current game"
                                  jsonAjax NoTimeout "GET" "currentGame" [] g $ \case
                                    Left (_, msg) -> return [SomeStoreAction (mkStore g) (GameError $ T.unpack msg)]
                                    Right g' -> return [SomeStoreAction (mkStore (GameState g' [])) (GameCreate g')]
                                  return g



cardsStore :: ReactStore GameState
cardsStore = do
  let g = runRand initGame (mkStdGen 0)
  mkStore (GameState (fst g) [])



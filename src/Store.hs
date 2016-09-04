{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase #-}
module Store where

import React.Flux
import React.Flux.Ajax
import React.Flux.Addons.Servant

import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Control.Monad.Random
import Data.Proxy
import Data.Aeson

import Model

data GameAction =
    GameSelect Card
  | GameGetCurrentGame
  | GameError String
  | GameWasUpdated Game
  | GameRequestNewGame
  deriving (Typeable, Generic, NFData)

-- data UpdatePending = NoUpdatePending | UpdatePending Text
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
                      GameError s -> do gameError s
                                        return (GameState g [])
                      GameSelect c -> gameSelectAction c
                      GameGetCurrentGame -> getCurrentGame >> (return gs)
                      GameWasUpdated g -> gameWasUpdated g
                      GameRequestNewGame -> gameRequestNewGame >> (return gs)
    putStrLn $ show newGameState
    return newGameState
      where gameRequestNewGame = do
              putStrLn "gameRequestNewGame"
              jsonAjax NoTimeout "GET" "newGame" [] g $ \case
                  Left (_, msg) -> return [SomeStoreAction cardsStore (GameError $ T.unpack msg)]
                  Right g' -> return [SomeStoreAction cardsStore (GameWasUpdated g')]

            getCurrentGame = do
              putStrLn $ "getCurrentGame"
              jsonAjax NoTimeout "GET" "currentGame" [] g $ \case
                  Left (_, msg) -> return [SomeStoreAction cardsStore (GameError $ T.unpack msg)]
                  Right g' -> return [SomeStoreAction cardsStore (GameWasUpdated g')]

            gameWasUpdated g = do
              putStrLn "gameWasUpdated"
              return (GameState g [])

            gameError s = do
              putStrLn "Error while performing ajax call"
              putStrLn s

            gameSelectAction c = do putStrLn $ "selected card: " ++ show c
                                    if c `elem` s
                                        then deselect c
                                        else select c
            deselect c = return $ GameState g (filter (/=c) s)
            select c = if length s == 2
                         then if isSolution (c, s !! 0, s !! 1)
                              then do
                                    let ng = (GameState (removeCards (c:s) g) [])
                                    putStrLn "GameSelect, new state:"
                                    jsonAjax NoTimeout "PUT" "setCurrentGame" [] (removeCards (c:s) g) $ \case
                                        Left (_, msg) -> return [SomeStoreAction cardsStore (GameError $ T.unpack msg)]
                                        Right g' -> return [SomeStoreAction cardsStore (GameWasUpdated g')]
                                    return ng
                              else do
                                    putStrLn "No solution, deselect all"
                                    return (GameState g [])
                         else return (GameState g (c:s))


cardsStore :: ReactStore GameState
cardsStore = do
  let g = runRand initGame (mkStdGen 0)
  mkStore (GameState (fst g) [])



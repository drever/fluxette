{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies #-}
module Routes where

import Servant
import Servant.HTML.Blaze

import Control.Concurrent.MVar
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Control.Monad

import Model

type FluxetteAPI = "js" :> Raw
               :<|> "css" :> Raw
               :<|> "html" :> Raw
               :<|> GetCurrentGame
               :<|> SetCurrentGame
               :<|> NewGame

type GetCurrentGame = "currentGame" :> Get '[JSON] Game
type SetCurrentGame = "setCurrentGame" :> ReqBody '[JSON] Game :> Post '[JSON] Game
type NewGame = "newGame" :> Post '[JSON] Game

server v = serveDirectory "../fluxette/js-build/install-root/bin/fluxette.jsexe"
    :<|> serveDirectory "webroot/css"
    :<|> serveDirectory "webroot/html"
    :<|> currentGame v
    :<|> setCurrentGame v
    :<|> newGame v

currentGame :: MVar Game -> EitherT ServantErr IO Game
currentGame v = do
  x <- liftIO $ readMVar v
  return x

setCurrentGame :: MVar Game -> Game -> EitherT ServantErr IO Game
setCurrentGame v g = do
  liftIO $ putStrLn "Set current game"
  liftIO $ putStrLn $ (show g)
  x <- liftIO $ takeMVar v
  liftIO $ putMVar v g
  return g

newGame :: MVar Game -> EitherT ServantErr IO Game
newGame v = do
  liftIO $ do
    putStrLn "New game"
    x <- takeMVar v
    g <- initGame
    putMVar v g
    return g


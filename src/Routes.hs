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

type GetCurrentGame = "currentGame" :> Get '[JSON] Game

server v = serveDirectory "../fluxette/js-build/install-root/bin/fluxette.jsexe"
    :<|> serveDirectory "webroot/css"
    :<|> serveDirectory "webroot/html"
    :<|> currentGame v

currentGame :: MVar Game -> EitherT ServantErr IO Game
currentGame v = do
  x <- liftIO $ readMVar v
  return x



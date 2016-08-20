module Dispatcher (
  dispatchGame
  ) where

import React.Flux
import Store

dispatchGame :: GameAction -> [SomeStoreAction]
dispatchGame a = [SomeStoreAction cardsStore a]


import React.Flux
import React.Flux.Ajax

import Views

main :: IO ()
main = do
  initAjax
  reactRender "fluxette" cardsApp ()
  -- reactRenderToString True cardsApp g >>= (\x -> putStrLn (T.unpack x))

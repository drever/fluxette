
import React.Flux
import Views

main :: IO ()
main = do
  reactRender "fluxette" cardsApp ()
  -- reactRenderToString True cardsApp g >>= (\x -> putStrLn (T.unpack x))

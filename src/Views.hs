{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Views (
  cardsApp
  ) where

import React.Flux

import Dispatcher
import Store

import Model

fillAlpha :: Fill -> String
fillAlpha Full = "1"
fillAlpha Half = "0.2"
fillAlpha Empty = "0"

jsColor :: Color -> String
jsColor Red = "rgb(255, 0, 0)"
jsColor Green = "rgb(0, 255, 0)"
jsColor Blue = "rgb(0, 0, 255)"

jsLightColor :: Color -> String
jsLightColor Red = "rgb(153, 0, 0)"
jsLightColor Green = "rgb(0, 153, 0)"
jsLightColor Blue = "rgb(0, 0, 153)"

cardsApp :: ReactView ()
cardsApp = defineControllerView "cards app" cardsStore $ \cardState () -> do
  div_ $ do
    svg_ [ "width" $= "300"
         , "height" $= "400" ] (mapM_ card_ $ (zip [0..] (cardSelection cardState)))
    button_ [(onClick $ \_ _ -> dispatchGame GameGetCurrentGame)] "Load current game"
    button_ [(onClick $ \_ _ -> dispatchGame GameRequestNewGame)] "New game"
  where cardSelection :: GameState -> [(Bool, Card)]
        cardSelection s = map (\c -> (c `elem` (gameStateSelection s), c)) (gameDealt $ gameStateGame s)

card :: ReactView (Int, (Bool, Card))
card = defineView "card" $ \(i, (s, c)) ->
  case c of
    Card c n Diamond f -> -- logo_ s i (c, f, n)
                          diamond_ s i (c, f, n)
    Card c n Box f -> box_ s i (c, f, n)
    Card c n Circle f -> Views.circle_ s i (c, f, n)

diamond :: Bool -> Int -> (Color, Fill, Number) -> ReactView ()
diamond s i fs@(c, f, n) = defineView "diamond" $ \() ->
    g_ ((onClick $ \_ _ -> dispatchGame (GameSelect (Card c n Diamond f))):cardProps i Diamond fs)
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

logo :: Bool -> Int -> (Color, Fill, Number) -> ReactView ()
logo s i fs@(c, f, n) = defineView "logo" $ \() ->
    g_ ((onClick $ \_ _ -> dispatchGame (GameSelect (Card c n Diamond f))):cardProps i Diamond fs)
      (boundingBox s (show i ++ show c ++ show f ++ show n) >>
        (case n of
          One -> haskellLogo cx 40 c f
          Two -> haskellLogo cx 27 c f >> haskellLogo cx 52 c f
          Three -> haskellLogo cx 15 c f >> haskellLogo cx 40 c f >> haskellLogo cx 65 c f))
            where cx = 23

haskellLogo :: Int -> Int -> Color -> Fill -> ReactElementM [SomeStoreAction] ()
haskellLogo x y c f = g_ ["transform" @= t] $ angle_ >> lambda_ >> equal_
  where lambda_ :: ReactElementM [SomeStoreAction] ()
        lambda_ = path_ ["strokeWidth" @= strokeWidth, "fill" @= jsLightColor c, "stroke" @= jsLightColor c, "fillOpacity" @= fillAlpha f, "transform" @= logoscale,
                        "d" @= unwords [
                         "M", "113.386719 340.15625"
                       , "L", "226.773438 170.078125"
                       , "L", "113.386719 0"
                       , "L", "198.425781 0"
                       , "L", "425.195312 340.15625"
                       , "L", "340.15625 340.15625"
                       , "L", "269.292969 233.859375"
                       , "L", "198.425781 340.15625"
                       , "L", "113.386719 340.15625"
                       , "Z"
                       , "M", "113.386719 340.15625"]] mempty
        equal_ :: ReactElementM [SomeStoreAction] ()
        equal_ = path_ ["strokeWidth" @= strokeWidth, "fill" @= jsColor c , "stroke" @= jsColor c, "fillOpacity" @= fillAlpha f, "transform" @= logoscale,
                       "d" @= unwords [
                            "M", "330.710938 155.90625"
                          , "L", "292.914062 99.214844"
                          , "L", "481.890625 99.210938"
                          , "L", "481.890625 155.90625"
                          , "L", "330.710938 155.90625"
                          , "Z"
                          , "M", "330.710938 155.90625"]] mempty >>
                 path_ ["strokeWidth" @= strokeWidth, "fill" @= jsColor c , "stroke" @= jsColor c, "fillOpacity" @= fillAlpha f, "transform" @= logoscale,
                        "d" @= unwords [
                             "M", "387.402344 240.945312"
                           , "L", "349.609375 184.253906"
                           , "L", "481.890625 184.25"
                           , "L", "481.890625 240.945312"
                           , "L", "387.402344 240.945312"
                           , "Z"
                           , "M", "387.402344 240.945312"]] mempty
        angle_ :: ReactElementM [SomeStoreAction] ()
        angle_ = path_ ["strokeWidth" @= strokeWidth, "fill" @= jsColor c , "stroke" @= jsColor c, "fillOpacity" @= fillAlpha f, "transform" @= logoscale,
                        "d" @= unwords [
                            "M", "0 340.15625"
                          , "L", "113.386719 170.078125"
                          , "L", "0 0"
                          , "L", "85.039062 0"
                          , "L", "198.425781 170.078125"
                          , "L", "85.039062 340.15625"
                          , "L", "0 340.15625"
                          , "Z"
                          , "M", "0 340.15625"]] mempty
        logoscale :: String
        logoscale = "scale(" ++ show logoscale' ++ ", " ++ show logoscale' ++ ")"
        logoscale' = 0.06
        strokeWidth = show (1.0 / logoscale')
        t = "translate(" ++ show x ++ ", " ++ show y ++ ")"

circle :: Bool -> Int -> (Color, Fill, Number) -> ReactView ()
circle s i (c, f, n) = defineView "circle" $ \() ->
    g_ ((onClick $ \_ _ -> dispatchGame (GameSelect (Card c n Circle f))):cardProps i Circle (c, f, n))
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
                                              , "key" @= ("circle" ++ show i ++ show x ++ show y)] mempty

box :: Bool -> Int -> (Color, Fill, Number) -> ReactView ()
box s i (c, f, n) = defineView "box" $ \() ->
    g_ ((onClick $ \_ _ -> dispatchGame (GameSelect (Card c n Box f))):cardProps i Box (c, f, n))
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


cardProps i s (c, f, n) = [
         "className" @= ("shape" ++ show s ++ " color" ++ show c ++ " fill" ++ show f ++ " number" ++ show n)
       , "key" @= (show i ++ show s ++ show c ++ show c ++ show f ++ show n)
       , "transform" @= t]
         where x = show $ i `mod` (3 :: Int) * 70
               y = show $ (floor (toRational i / 3.0)) `mod` (4 :: Int) * 100
               t = "translate(" ++ x ++ "," ++ y ++ ")"

boundingBox :: Bool -> String -> ReactElementM [SomeStoreAction] ()
boundingBox s k = rect_ [ "key" @= k
                        , "stroke" $= "black"
                        , "fill" $= if not s then "white" else "rgb(200, 250, 250)"
                        , "strokeWidth" $= "2"
                        , "x" $= "5"
                        , "y" $= "5"
                        , "width" $= "60"
                        , "height" $= "90"] mempty

box_ :: Bool -> Int -> (Color, Fill, Number) -> ReactElementM eventHandler ()
box_ s i fs = view (box s i fs) () mempty

card_ :: (Int, (Bool, Card)) -> ReactElementM eventHandler ()
card_ !a@(i, (s, c)) = viewWithIKey card i a mempty

diamond_ :: Bool -> Int -> (Color, Fill, Number) -> ReactElementM eventHandler ()
diamond_ s i fs = view (diamond s i fs) () mempty

logo_ :: Bool -> Int -> (Color, Fill, Number) -> ReactElementM eventHandler ()
logo_ s i fs = view (logo s i fs) () mempty

circle_ :: Bool -> Int -> (Color, Fill, Number) -> ReactElementM eventHandler ()
circle_ s i fs = view (circle s i fs) () mempty


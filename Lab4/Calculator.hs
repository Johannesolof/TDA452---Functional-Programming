import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Maybe

import Pages

import Expr



canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Elem -> Canvas -> IO ()
readAndDraw fi si can = do
  fs <- getProp fi "value"
  ss <- getProp si "value"
  render can $ drawFun fs ss
  return ()

drawFun :: String -> String -> Picture ()
drawFun fs ss = case readExpr fs of
  Just e -> stroke . path $ points e (read ss) (canWidth, canHeight)
  _      -> text (0,0) "" -- Draw nothing when invalid expression

diffFun :: Elem -> IO ()
diffFun fi = do
  fs <- getProp fi "value"
  setProp fi "value" $ showExpr $ differentiate $ fromJust $ readExpr fs

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight    -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="   -- The text "f(x)="
    scl     <- mkHTML "<i>scale</i>="
    fInput  <- mkInput 20 "x"                 -- The formula input
    sInput  <- mkInput 20 "0.04"
    draw    <- mkButton "Draw graph"          -- The draw button
    diff    <- mkButton "Differentiate"        -- The differentiate button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    scale <- mkDiv
    buttons <- mkDiv
    row formula [fx,fInput]
    row scale   [scl,sInput]
    row buttons [diff, draw]
    column documentBody [canvas,formula,scale,buttons]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle fInput "fontSize" "14pt"
    setStyle sInput "fontSize" "14pt"
    focus fInput
    select fInput

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_     -> readAndDraw fInput sInput can
    onEvent diff  Click $ \_     -> diffFun fInput
    onEvent fInput KeyUp $ \code ->
      when (code==13) $ readAndDraw fInput sInput can
    -- "Enter" key has code 13


points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (w,h) = points' [] 0
  where
    pixToReal x = (x - 0.5 * fromIntegral w) * s
    realToPix y = (-y + s * 0.5 * fromIntegral h) / s
    points' ps x | x <= w    = points' (p:ps) (x+1)
                 | otherwise = reverse ps
      where
        x' = fromIntegral x
        p  = (x', realToPix $ eval e (pixToReal x'))

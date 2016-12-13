import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

import Pages

import Expr



canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw = undefined

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13


-- * Assignment H

points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (w,h) = points' [] 0
  where
    pixToReal :: Double -> Double
    pixToReal x =  x - s / 2 * fromIntegral w
    realToPix :: Double -> Double
    realToPix y = -y + s / 2 * fromIntegral h
    points' ps x | x <= w    = points' (p:ps) (x+1)
                 | otherwise = reverse ps
      where
        x' = fromIntegral x
        p  = (x', realToPix $ eval e (pixToReal x'))

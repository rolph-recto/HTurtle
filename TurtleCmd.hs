-- turtleCmd.hs
-- interpreter for turtle command language

module TurtleCmd (
  execTurtleCmd
) where

import Control.Monad.State
import Graphics.Gloss (Color)

import TurtleState
import TurtleExpr

-- draw a line (duh)
drawLine :: (Float, Float) -> (Float, Float) -> Color -> CanvasShape
drawLine (oldx,oldy) (newx,newy) c =
  CanvasLine {
    x1 = oldx
  , y1 = oldy
  , x2 = newx
  , y2 = newy
  , color = c
  }

roundAngle :: Float -> Float
roundAngle angle =
  if angle < 0
  then angle + 360.0
  else if angle > 360.0
       then angle - 360.0
       else angle

moveTurtle :: Int -> Float -> State TurtleState String
moveTurtle steps direction = do
  -- steps must be nonnegative
  if steps < 0
  then do
    return "Steps must be non-negative"
  else do
    tstate <- get
    -- subtract 90 from angle since
    -- then convert to radians
    let radAngle = (((tangle tstate) + 90.0) / 360.0) * 2 * pi
    let x = tx tstate
    let y = ty tstate
    let fsteps = fromIntegral steps :: Float
    let x' = x + fsteps * cos radAngle * direction
    let y' = y + fsteps * sin radAngle * direction

    -- if pen is down, draw a path
    if pen tstate 
    then do
      let line = drawLine (x,y) (x',y') (penColor tstate)
      let shapes' = line:(shapes tstate)
      let tstate' = tstate { tx=x', ty=y', shapes=shapes' }
      put tstate'
      return ""
    else do
      let tstate' = tstate { tx=x', ty=y' }
      put tstate'
      return ""

execTurtleCmd :: TurtleExpr -> State TurtleState String
execTurtleCmd (Forward steps) = do
  log <- moveTurtle steps 1.0
  return log

execTurtleCmd (Back steps) = do
  log <- moveTurtle steps (-1.0)
  return log

-- rotate turtle rightward
execTurtleCmd (TurnRight delta) = do
  -- steps must be nonnegative
  if delta < 0
  then do
    return "Angle must be non-negative"
  else do
    tstate <- get

    let angle' = (tangle tstate) - (fromIntegral delta :: Float)
    let tstate' = tstate { tangle=roundAngle angle' }
    put tstate'
    
    return ""

-- rotate turtle leftward
execTurtleCmd (TurnLeft delta) = do
  -- steps must be nonnegative
  if delta < 0
  then do
    return "Angle must be non-negative"
  else do
    tstate <- get

    let angle' = (tangle tstate) + (fromIntegral delta :: Float)
    let tstate' = tstate { tangle=roundAngle angle' }
    put tstate'
    
    return ""
  
execTurtleCmd _ =  do
  return ""


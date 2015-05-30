-- turtleCmd.hs
-- interpreter for turtle command language

module TurtleCmd (
  execTurtleCmd
) where

import Control.Monad.State
import Data.String (unlines)
import Graphics.Gloss (Color, makeColorI)

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

turnTurtle :: Int -> Float -> State TurtleState String
turnTurtle delta direction = do
  -- steps must be nonnegative
  if delta < 0
  then do
    return "Angle must be non-negative"
  else do
    tstate <- get

    let angle' = (tangle tstate) + (fromIntegral delta :: Float) * direction
    let tstate' = tstate { tangle=roundAngle angle' }
    put tstate'
    
    return ""

-- execute turtle langage commands (evals AST)
execTurtleCmd :: TurtleExpr -> State TurtleState String
execTurtleCmd cmd = case cmd of
  -- move turtle forward
  Forward steps -> do
    log <- moveTurtle steps 1.0
    return log

  -- move turtle backward
  Back steps -> do
    log <- moveTurtle steps (-1.0)
    return log

  -- rotate turtle rightward
  TurnRight delta -> do
    log <- turnTurtle delta (-1.0)
    return log

  -- rotate turtle leftward
  TurnLeft delta -> do
    log <- turnTurtle delta 1.0
    return log

  -- draw a circle
  DrawCircle r -> do
    if r < 0
    then do
      return "Radius must be non-negative"
    else do
      tstate <- get
      if pen tstate
      then do
        let c = CanvasCircle {
                  rad=fromIntegral r :: Float
                , cx=tx tstate
                , cy=ty tstate
                , color=penColor tstate
                }
        put $ tstate { shapes=c:(shapes tstate) }
        return ""
      else
        return ""

  -- turtle doesn't draw
  PenUp -> do
    tstate <- get
    put $ tstate { pen=False }
    return ""

  -- turtle draws
  PenDown -> do
    tstate <- get
    put $ tstate { pen=True }
    return ""

  -- set x coord of turtle
  SetX x -> do
    tstate <- get
    put $ tstate { tx=fromIntegral x :: Float }
    return ""
    
  -- set y coord of turtle
  SetY y -> do
    tstate <- get
    put $ tstate { ty=fromIntegral y :: Float }
    return ""

  -- set turtle coords
  SetXY x y -> do
    tstate <- get
    put $ tstate { tx=fromIntegral x :: Float, ty=fromIntegral y :: Float }
    return ""

  -- set turtle angle
  SetAngle angle -> do
    tstate <- get
    put $ tstate { tangle=roundAngle $ fromIntegral angle :: Float }
    return ""

  -- move turtle to original position
  Home -> do
    tstate <- get
    put $ tstate { tx=0.0, ty=0.0, tangle=0.0 }
    return ""

  -- set pen color
  PenColor r g b a -> do
    if checkColor r && checkColor g && checkColor b && checkColor a
    then do
      tstate <- get
      put $ tstate { penColor=makeColorI r g b a }
      return ""
    else do
      return "Color components must be between [0,255]"
    where checkColor c = c >= 0 && c <= 255

  -- draw the turtle
  ShowTurtle -> do
    tstate <- get
    put $ tstate { tshow=True }
    return ""

  -- don't draw the turtle
  HideTurtle -> do
    tstate <- get
    put $ tstate { tshow=False }
    return ""

  -- clear the screen
  Clear -> do
    tstate <- get
    put $ tstate { shapes=[] }
    return ""

  Seq seq -> do
    logs <- forM seq execTurtleCmd
    return $ unlines logs

  -- loop over a sequence of commands
  Repeat n seq -> do
    if n > 0
    then do
      logs <- execTurtleCmd $ Seq seq
      tailLogs <- execTurtleCmd (Repeat (n-1) seq)
      return $ logs ++ tailLogs
    else do
      return ""


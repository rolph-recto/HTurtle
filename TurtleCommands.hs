module TurtleCommands (turtleCommands) where

import Control.Monad.Except
import Control.Monad.State

import Graphics.Gloss (Color, makeColorI)

import HLispExpr
import HLispEval

import TurtleState
import TurtleDraw

-- draw a line (duh)
drawLine :: (Float, Float) -> (Float, Float) -> Color -> CanvasShape
drawLine (oldx,oldy) (newx,newy) c =
  CanvasLine { x1 = oldx, y1 = oldy, x2 = newx, y2 = newy, color = c }

moveTurtle :: Int -> Bool -> LispExec DrawState
moveTurtle steps forward = do
  let direction = if forward then 1.0 else -1.0
  if steps < 0
  then do
    throwError "steps must be non-negative"

  else do
    (dstate, env) <- lift get
  
    -- subtract 90 from angle since
    -- then convert to radians
    let radAngle = (((tangle dstate) + 90.0) / 360.0) * 2 * pi
    let x = tx dstate
    let y = ty dstate
    let fsteps = fromIntegral steps :: Float
    let x' = x + fsteps * cos radAngle * direction
    let y' = y + fsteps * sin radAngle * direction

    -- if pen is down, draw a path
    if pen dstate 
    then do
      let line = drawLine (x,y) (x',y') (penColor dstate)
      let shapes' = line:(shapes dstate)
      let dstate' = dstate { tx=x', ty=y', shapes=shapes' }
      lift $ put (dstate', env)
      return LispUnit

    else do
      let dstate' = dstate { tx=x', ty=y' }
      lift $ put (dstate', env)
      return LispUnit

fd :: PrimFunc DrawState
fd env (steps:_) = do
  stepVal <- eval env steps
  case stepVal of
    LispNum n -> moveTurtle n True
    otherwise -> throwError "fd expects a num argument"

turtleCommands :: [(String, (Int, PrimFunc DrawState))]
turtleCommands = [("fd", (1, fd))]

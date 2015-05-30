module TurtleDraw (
  drawTurtleState
) where

import Control.Monad.State
import Graphics.Gloss.Interface.IO.Game hiding (color)

import TurtleState

drawCanvasShape :: CanvasShape -> Picture
drawCanvasShape shape =
  case shape of 
    CanvasLine x1 y1 x2 y2 color ->
      Color color $ Line [(x1,y1), (x2,y2)]
    CanvasCircle rad cx cy color ->
      Color color $ Translate cx cy $ Circle rad

-- draw all shapes in the canvas
drawCanvasShapes :: State TurtleState [Picture]
drawCanvasShapes = do
  tstate <- get
  return $ map drawCanvasShape $ shapes tstate

-- draw the turtle
drawTurtle :: State TurtleState [Picture]
drawTurtle = do
  tstate <- get
  let x = tx tstate
  let y = ty tstate
  let angle = tangle tstate
  let rangle = (tangle tstate) / 360 * 2 * pi
  let size = tsize tstate
  let rotate (x',y') = ((cos rangle)*x'-(sin rangle)*y', (sin rangle)*x'+(cos rangle)*y')
  let translate (x',y') = (x'+x,y'+y)
  let setCoord = translate . rotate
  let p1 = setCoord (-size/2, -size/2)
  let p2 = setCoord (size/2, -size/2)
  let p3 = setCoord (0, size/2)
  let triangle = [ Line [p1,p2], Line [p1,p3], Line [p2,p3] ]
  return [Color white $ Pictures triangle]

-- draw everything!
drawCanvas :: State TurtleState Picture
drawCanvas = do
  shapes <- drawCanvasShapes
  turtle <- drawTurtle
  return $ Pictures (shapes ++ turtle)

-- drawing callback to playIO
drawTurtleState :: TurtleState -> IO Picture
drawTurtleState tstate = 
  return $ evalState drawCanvas tstate

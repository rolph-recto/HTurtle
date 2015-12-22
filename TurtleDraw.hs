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
  (dstate, _) <- get
  return $ map drawCanvasShape $ shapes dstate

-- draw the turtle
drawTurtle :: State TurtleState [Picture]
drawTurtle = do
  (dstate, _) <- get
  let x = tx dstate
  let y = ty dstate
  let angle = tangle dstate
  let rangle = (tangle dstate) / 360 * 2 * pi
  let size = tsize dstate
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
  (dstate, _) <- get
  shapes <- drawCanvasShapes
  if tshow dstate
  then do
    turtle <- drawTurtle
    return $ Pictures (shapes ++ turtle)
  else
    return $ Pictures shapes

-- drawing callback to playIO
drawTurtleState :: TurtleState -> IO Picture
drawTurtleState tstate  = 
  return $ evalState drawCanvas tstate

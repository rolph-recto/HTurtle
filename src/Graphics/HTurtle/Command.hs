module Graphics.HTurtle.Command (turtleCommands) where

import Control.Monad.Except
import Control.Monad.State

import Graphics.Gloss (Color, makeColor)

import Language.HLisp.Expr
import Language.HLisp.Eval

import Graphics.HTurtle.State
import Graphics.HTurtle.Draw

-- draw a line (duh)
drawLine :: (Float, Float) -> (Float, Float) -> Color -> CanvasShape
drawLine (oldx,oldy) (newx,newy) c =
  CanvasLine { x1 = oldx, y1 = oldy, x2 = newx, y2 = newy, color = c }

roundAngle :: Float -> Float
roundAngle angle =
  if angle < 0
  then angle + 360.0
  else if angle > 360.0
       then angle - 360.0
       else angle

moveTurtle :: Int -> Float -> LispExec DrawState
moveTurtle steps direction = do
  if steps < 0
  then do
    throwError "steps must be non-negative"

  else do
    dstate <- getUserState
  
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
      putUserState dstate'
      return LispUnit

    else do
      let dstate' = dstate { tx=x', ty=y' }
      putUserState dstate'
      return LispUnit

turnTurtle :: Int -> Float -> LispExec DrawState
turnTurtle delta direction = do
  -- steps must be nonnegative
  if delta < 0
  then do
    throwError "Angle must be non-negative"
  else do
    dstate <- getUserState

    let angle' = (tangle dstate) + (fromIntegral delta :: Float) * direction
    let dstate' = dstate { tangle=roundAngle angle' }
    putUserState dstate'
    return LispUnit

forward :: PrimFunc DrawState
forward env (steps:_) = do
  stepVal <- eval env steps
  case stepVal of
    LispNum n -> moveTurtle n 1.0
    otherwise -> throwError "forward expects a num argument"

back :: PrimFunc DrawState
back env (steps:_) = do
  stepVal <- eval env steps
  case stepVal of
    LispNum n -> moveTurtle n (-1.0)
    otherwise -> throwError "back expects a num argument"

turnRight :: PrimFunc DrawState
turnRight env (delta:_) = do
  deltaVal <- eval env delta
  case deltaVal of
    LispNum n -> turnTurtle n (-1.0)
    otherwise -> throwError "right expects a num argument"

turnLeft :: PrimFunc DrawState
turnLeft env (delta:_) = do
  deltaVal <- eval env delta
  case deltaVal of
    LispNum n -> turnTurtle n 1.0
    otherwise -> throwError "throwError expects a num argument"

circle :: PrimFunc DrawState
circle env (rad:_) = do
  radVal <- eval env rad
  case radVal of
    LispNum r -> do
      if r < 0
      then do
        throwError "radius must be non-negative"
      else do
        dstate <- getUserState
        if pen dstate
        then do
          let c = CanvasCircle {
                    rad=fromIntegral r :: Float
                  , cx=tx dstate
                  , cy=ty dstate
                  , color=penColor dstate
                  }
          let dstate' = dstate { shapes=c:(shapes dstate) }
          putUserState dstate'
          return LispUnit

        else do
          return LispUnit

arc :: PrimFunc DrawState
arc env (rad:s:t:_) = do
  radVal <- eval env rad
  sval <- eval env s
  tval <- eval env t
  case (radVal,sval,tval) of
    (LispNum r, LispNum s', LispNum t') -> do
      if r < 0
      then do
        throwError "radius must be non-negative"
      else do
        dstate <- getUserState
        if pen dstate
        then do
          let c = CanvasArc {
                    rad = fromIntegral r :: Float
                  , cx = tx dstate
                  , cy = ty dstate
                  , arcStart = fromIntegral s' :: Float
                  , arcEnd = fromIntegral t' :: Float
                  , arcRot = tangle dstate
                  , color = penColor dstate
                  }
          let dstate' = dstate { shapes=c:(shapes dstate) }
          putUserState dstate'
          return LispUnit

        else do
          return LispUnit

penUp :: PrimFunc DrawState
penUp env args = do
  dstate <- getUserState
  let dstate' = dstate { pen=False }
  putUserState dstate'
  return LispUnit

penDown :: PrimFunc DrawState
penDown env args = do
  dstate <- getUserState
  let dstate' = dstate { pen=True }
  putUserState dstate'
  return LispUnit

setX :: PrimFunc DrawState
setX env (x':_) = do
  xval <- eval env x'
  case xval of
    LispNum x -> do
      dstate <- getUserState
      let dstate' = dstate { tx=fromIntegral x :: Float }
      putUserState dstate'
      return LispUnit
  
    otherwise -> throwError "setX expects a num argument"

setY :: PrimFunc DrawState
setY env (y':_) = do
  yval <- eval env y'
  case yval of
    LispNum y -> do
      dstate <- getUserState
      let dstate' = dstate { ty=fromIntegral y :: Float }
      putUserState dstate'
      return LispUnit
  
    otherwise -> throwError "setY expects a num argument"

setXY :: PrimFunc DrawState
setXY env (x':y':_) = do
  xval <- eval env x'
  yval <- eval env y'
  case (xval, yval) of
    (LispNum x, LispNum y) -> do
      dstate <- getUserState
      let dstate' = dstate { tx=fromIntegral x :: Float, ty=fromIntegral y :: Float }
      putUserState dstate'
      return LispUnit
  
    otherwise -> throwError "setXY expects num arguments"

setAngle :: PrimFunc DrawState
setAngle env (angle':_) = do
  angleVal <- eval env angle'
  case angleVal of
    LispNum angle -> do
      dstate <- getUserState
      let dstate' = dstate { tangle=roundAngle $ fromIntegral angle :: Float }
      putUserState dstate'
      return LispUnit
  
    otherwise -> throwError "setAngle expects a num argument"

home :: PrimFunc DrawState
home env args = do
  dstate <- getUserState
  let dstate' = dstate { tx=0.0, ty=0.0, tangle=0.0 }
  putUserState dstate'
  return LispUnit

clear :: PrimFunc DrawState 
clear env args = do
  dstate <- getUserState
  let dstate' = dstate { shapes=[] }
  putUserState dstate'
  return LispUnit

reset :: PrimFunc DrawState
reset env args = do
  clear env args
  home env args

setColor :: PrimFunc DrawState
setColor env (r':g':b':a':_) = do
  rval <- eval env r'
  gval <- eval env g'
  bval <- eval env b'
  aval <- eval env a'
  case (rval,gval,bval,aval) of
    (LispNum r, LispNum g, LispNum b, LispNum a) -> do
      if checkColor r && checkColor g && checkColor b && checkColor a
      then do
        dstate <- getUserState
        let (r',g',b',a') = (fromIntegral r/255 :: Float,
                             fromIntegral g/255 :: Float,
                             fromIntegral b/255 :: Float,
                             fromIntegral a/255 :: Float)
        let dstate' = dstate { penColor=makeColor r' g' b' a' }
        putUserState dstate'
        return LispUnit

      else throwError "color components must be between [0,255]"

    otherwise -> throwError "color takes in num arguments"

  where checkColor c = c >= 0 && c <= 255

showTurtle :: PrimFunc DrawState
showTurtle env args = do
  dstate <- getUserState
  let dstate' = dstate { tshow=True }
  putUserState dstate'
  return LispUnit

hideTurtle :: PrimFunc DrawState
hideTurtle env args = do
  dstate <- getUserState
  let dstate' = dstate { tshow=False }
  putUserState dstate'
  return LispUnit

turtleCommands :: [(String, (Int, PrimFunc DrawState))]
turtleCommands = [
  ("forward", (1, forward)),
  ("fd", (1, forward)),
  ("back", (1, back)),
  ("bk", (1, back)),
  ("right", (1, turnRight)),
  ("rt", (1, turnRight)),
  ("left", (1, turnLeft)),
  ("lt", (1, turnLeft)),
  ("circle", (1, circle)),
  ("arc", (3, arc)),
  ("penup", (0, penUp)),
  ("pu", (0, penUp)),
  ("pendown", (0, penDown)),
  ("pd", (0, penDown)),
  ("setx", (1, setX)),
  ("sety", (1, setY)),
  ("setxy", (2, setXY)),
  ("setangle", (1, setAngle)),
  ("home", (0, home)),
  ("clear", (0, clear)),
  ("reset", (0, reset)),
  ("color", (4, setColor)),
  ("pen", (4, setColor)),
  ("pencolor", (4, setColor)),
  ("showturtle", (0, showTurtle)),
  ("show", (0, showTurtle)),
  ("hideturtle", (0, hideTurtle)),
  ("hide", (0, hideTurtle))]



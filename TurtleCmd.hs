-- turtleCmd.hs
-- interpreter for turtle command language

module TurtleCmd (execTurtleCmd) where 

import Graphics.Gloss (Color, makeColorI)

import Control.Monad.State
import EitherT

import Data.String (unlines)
import qualified Data.Map.Strict as M

import TurtleDraw
import TurtleExpr

type TurtleEnv = M.HashMap String TurtleVal

-- (draw state, activation records, heap)
type TurtleState = (DrawState, [TurtleEnv], TurtleEnv)

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

-- helper functions to convert Turtle values to Haskell values
-- exceptions thrown here should be unreachable
-- by type-checking expressions before executing them
toTurtleNum :: TurtleValue -> Int
toTurtleNum (TurtleNum n) = n
toTurtleNum _             = error "value is not TurtleNum"

toTurtleBool :: TurtleValue -> Bool
toTurtleBool (TurtleBool b) = b
toTurtleBool _              = error "value is not TurtleBool"

toTurtleUnit :: TurtleValue -> ()
toTurtleUnit TurtleUnit = ()
toTurtleUnit _          = error "value is not TurtleUnit"

lget = lift get
lput = lift . put

moveTurtle :: TurtleExpr -> Float -> EitherT String (State TurtleState) TurtleValue
moveTurtle steps' direction = do
  ret <- execTurtleCmd steps'
  let steps = toTurtleNum ret
  -- steps must be nonnegative
  if steps < 0
  then do
    failEitherT "Steps must be non-negative"
  else do
    (dstate,env) <- lget
    -- subtract 90 from angle since
    -- then convert to radians
    let radAngle = (((tangle dstate) + 90.0) / 360.0) * 2 * pi
    let x = tx dstate
    let y = ty dstate
    let fsteps = fromIntegral steps :: Float
    let x' = x + fsteps * cos radAngle * direction
    let y' = y + fsteps * sin radAngle * direction

    -- if pen is down, draw a path
    if pen tstate 
    then do
      let line = drawLine (x,y) (x',y') (penColor tstate)
      let shapes' = line:(shapes tstate)
      let dstate' = dstate { tx=x', ty=y', shapes=shapes' }
      lput (tstate',env)
      return TurtleUnit
    else do
      let dstate' = dstate { tx=x', ty=y' }
      lput (dstate',env)
      return TurtleUnit

turnTurtle :: TurtleExpr -> Float -> EitherT String (State TurtleState) TurtleValue
turnTurtle delta' direction = do
  ret <- execTurtleCmd delta'
  let delta = toTurtleNum ret
  -- steps must be nonnegative
  if delta < 0
  then do
    failEitherT "Angle must be non-negative"
  else do
    (tstate,_) <- lget

    let angle' = (tangle tstate) + (fromIntegral delta :: Float) * direction
    let tstate' = tstate { tangle=roundAngle angle' }
    lput tstate'
    
    return TurtleUnit

-- interprets turtle langage commands (evals AST)
-- this implements big-step operational semantics
execTurtleCmd :: TurtleEnv -> TurtleExpr -> EitherT String (State TurtleState) TurtleValue
execTurtleCmd env cmd = case cmd of
  -- move turtle forward
  Forward steps -> do
    moveTurtle steps 1.0

  -- move turtle backward
  Back steps -> do
    moveTurtle steps (-1.0)

  -- rotate turtle rightward
  TurnRight delta -> do
    turnTurtle delta (-1.0)

  -- rotate turtle leftward
  TurnLeft delta -> do
    turnTurtle delta 1.0

  -- draw a circle
  DrawCircle r' -> do
    ret <- execTurtleCmd r'
    let r = toTurtleNum ret
    if r < 0
    then do
      failEitherT "Radius must be non-negative"
    else do
      (tstate,_) <- lget
      if pen tstate
      then do
        let c = CanvasCircle {
                  rad=fromIntegral r :: Float
                , cx=tx tstate
                , cy=ty tstate
                , color=penColor tstate
                }
        lput $ tstate { shapes=c:(shapes tstate) }
        return TurtleUnit
      else do
        return TurtleUnit

  -- turtle doesn't draw
  PenUp -> do
    (dstate, env) <- lget
    lput (dstate { pen=False }, env)
    return TurtleUnit

  -- turtle draws
  PenDown -> do
    tstate <- lget
    lput $ tstate { pen=True }
    return TurtleUnit

  -- set x coord of turtle
  SetX x' -> do
    ret <- execTurtleCmd x'
    let x = toTurtleNum ret
    tstate <- lget
    lput $ tstate { tx=fromIntegral x :: Float }
    return TurtleUnit
    
  -- set y coord of turtle
  SetY y' -> do
    ret <- execTurtleCmd y'
    let y = toTurtleNum ret
    tstate <- lget
    lput $ tstate { ty=fromIntegral y :: Float }
    return TurtleUnit

  -- set turtle coords
  SetXY x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    tstate <- lget
    lput $ tstate { tx=fromIntegral x :: Float, ty=fromIntegral y :: Float }
    return TurtleUnit

  -- set turtle angle
  SetAngle angle' -> do
    ret <- execTurtleCmd angle'
    let angle = toTurtleNum ret
    tstate <- lget
    lput $ tstate { tangle=roundAngle $ fromIntegral angle :: Float }
    return TurtleUnit

  -- move turtle to original position
  Home -> do
    tstate <- lget
    lput $ tstate { tx=0.0, ty=0.0, tangle=0.0 }
    return TurtleUnit

  -- set pen color
  PenColor r' g' b' a' -> do
    retR <- execTurtleCmd r'
    retG <- execTurtleCmd g'
    retB <- execTurtleCmd b'
    retA <- execTurtleCmd a'
    let r = toTurtleNum retR
    let g = toTurtleNum retG
    let b = toTurtleNum retB
    let a = toTurtleNum retA
    if checkColor r && checkColor g && checkColor b && checkColor a
    then do
      tstate <- lget
      lput $ tstate { penColor=makeColorI r g b a }
      return TurtleUnit
    else do
      failEitherT "Color components must be between [0,255]"
    where checkColor c = c >= 0 && c <= 255

  -- draw the turtle
  ShowTurtle -> do
    tstate <- lget
    lput $ tstate { tshow=True }
    return TurtleUnit

  -- don't draw the turtle
  HideTurtle -> do
    tstate <- lget
    lput $ tstate { tshow=False }
    return TurtleUnit

  -- clear the screen
  Clear -> do
    tstate <- lget
    lput $ tstate { shapes=[] }
    return TurtleUnit

  -- perform a sequence of commands
  -- return the value of the last command
  Seq seq -> do
    retlist <- sequence $ map execTurtleCmd seq
    return $ last retlist

  -- loop over a sequence of commands
  Repeat n' seq -> do
    retN <- execTurtleCmd n'
    let n = toTurtleNum retN
    if n > 0
    then do
      execTurtleCmd $ Seq seq
      execTurtleCmd (Repeat (Num (n-1)) seq)
      return TurtleUnit
    else do
      return TurtleUnit

  If pred' thenBranch elseBranch -> do
    ret <- execTurtleCmd pred'
    let pred = toTurtleBool ret
    if pred
    then do
      execTurtleCmd thenBranch
    else do
      execTurtleCmd elseBranch

  Return retExpr -> do
    retval <- execTurtleCmd retExpr
    return retval
  
  Num n -> do
    return $ TurtleNum n

  Add x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    return $ TurtleNum (x+y)

  Sub x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    return $ TurtleNum (x-y)

  Mul x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    return $ TurtleNum (x*y)

  Div x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    if y == 0
    then do
      failEitherT "Division by 0!"
    else do
      return $ TurtleNum (x `div` y)

  BoolTrue -> do
    return $ TurtleBool True

  BoolFalse -> do
    return $ TurtleBool False

  And x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleBool retX
    let y = toTurtleBool retY
    return $ TurtleBool (x && y)

  Or x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleBool retX
    let y = toTurtleBool retY
    return $ TurtleBool (x || y)

  Not x' -> do
    retX <- execTurtleCmd x'
    let x = toTurtleBool retX
    return $ TurtleBool (not x)

  Eq x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    return $ TurtleBool (x == y)

  Leq x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    return $ TurtleBool (x <= y)

  Geq x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    return $ TurtleBool (x >= y)

  Lt x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    return $ TurtleBool (x < y)

  Gt x' y' -> do
    retX <- execTurtleCmd x'
    retY <- execTurtleCmd y'
    let x = toTurtleNum retX
    let y = toTurtleNum retY
    return $ TurtleBool (x > y)

  Var s -> do


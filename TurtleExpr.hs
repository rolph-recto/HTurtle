-- TurtleExpr
-- definition and parser for turtle command language

module TurtleExpr (
  TurtleExpr(..)
, TurtleValue(..)
-- , parseTurtle
, typeTurtle
) where

import Data.Char
import Data.String.Utils
import Control.Monad

import Text.ParserCombinators.Parsec

data TurtleExpr = Forward TurtleExpr                                    -- move turtle forward
                | Back TurtleExpr                                       -- move turtle backward
                | TurnRight TurtleExpr                                  -- rotate turtle rightward
                | TurnLeft TurtleExpr                                   -- rotate turtle leftward
                | DrawCircle TurtleExpr                                 -- draw a circle of some radius
                | PenUp                                                 -- turtle doesn't draw
                | PenDown                                               -- turtle draws again
                | SetX TurtleExpr                                       -- set turtle x-coord
                | SetY TurtleExpr                                       -- set turtle y-coord
                | SetXY TurtleExpr TurtleExpr                           -- set turtle coords
                | SetAngle TurtleExpr                                   -- set turtle angle
                | Home                                                  -- move turtle to (0,0)
                | PenColor TurtleExpr TurtleExpr TurtleExpr TurtleExpr  -- set pen color
                | ShowTurtle                                            -- show the turtle
                | HideTurtle                                            -- hide the turtle
                | Clear                                                 -- clear the screen
                | Seq [TurtleExpr]                                      -- sequence of commands
                | Repeat TurtleExpr [TurtleExpr]                        -- repeat a sequence of commands
                | If TurtleExpr TurtleExpr TurtleExpr                   -- conditional
                -- ARITHMETIC
                | Num Int                   
                | Add TurtleExpr TurtleExpr 
                | Sub TurtleExpr TurtleExpr
                | Mul TurtleExpr TurtleExpr
                -- | Div TurtleExpr TurtleExpr
                -- we're going to ignore Div for now since
                -- we want to bypass div by zero issues and
                -- the fact that it is not closed under the naturals
                -- Boolean
                | BoolTrue
                | BoolFalse
                | And TurtleExpr TurtleExpr
                | Or TurtleExpr TurtleExpr
                | Not TurtleExpr
                | Eq TurtleExpr TurtleExpr
                | Leq TurtleExpr TurtleExpr
                | Geq TurtleExpr TurtleExpr
                | Lt TurtleExpr TurtleExpr
                | Gt TurtleExpr TurtleExpr
                deriving (Show)

-- values that turtle expressions can evaluate to
data TurtleValue = TurtleNum Int | TurtleBool Bool | TurtleUnit

{--
-- PARSER

turtleSeq = do
  exprs <- sepBy1 turtleExpr spaces
  if length exprs == 1
  then do
    return $ head exprs
  else do
    return $ Seq exprs

turtleExpr =  try forward
          <|> try back
          <|> try right
          <|> try left
          <|> try drawCircle
          <|> try penUp
          <|> try penDown
          <|> try setX
          <|> try setY
          <|> try setXY
          <|> try setAngle
          <|> try home
          <|> try penColor
          <|> try showTurtle
          <|> try hideTurtle
          <|> try clear
          <|> try turtleRepeat
          <|> try turtleIf
          <|> try turtleNum
          <|> try turtleAdd
          <|> try turtleSub
          <|> try turtleMul
          <|> try turtleTrue
          <|> try turtleFalse
          <|> try turtleAnd
          <|> try turtleOr
          <|> try turtleNot
          <|> try turtleEq
          <|> try turtleLeq
          <|> try turtleGeq
          <|> try turtleLt
          <|> turtleGt


forward = do
  (try $ string "forward") <|> (try $ string "fd") <|> string "f"
  spaces
  stepstr <- many1 digit
  let steps = read stepstr :: Int
  return $ Forward steps

back = do
  (try $ string "back") <|> (try $ string "bk") <|> string "b"
  spaces
  stepstr <- many1 digit
  let steps = read stepstr :: Int
  return $ Back steps

right = do
  (try $ string "right") <|> (try $ string "rt") <|> string "r"
  spaces
  anglestr <- many1 digit
  let angle = read anglestr :: Int
  return $ TurnRight angle

left = do
  (try $ string "left") <|> (try $ string "lt") <|> string "l"
  spaces
  anglestr <- many1 digit
  let angle = read anglestr :: Int
  return $ TurnLeft angle

drawCircle = do
  (try $ string "circle") <|> string "cir"
  spaces
  radstr <- many1 digit
  let radius = read radstr :: Int
  return $ DrawCircle radius

penUp = do
  (try $ string "penup") <|> (try $ string "pu") <|> string "up"
  return PenUp

penDown = do
  (try $ string "pendown") <|> (try $ string "pd") <|> string "down"
  return PenDown

setX = do
  (try $ string "setx") <|> string "x"
  spaces
  posstr <- many1 digit
  let pos = read posstr :: Int
  return $ SetX pos

setY = do
  (try $ string "sety") <|> string "y"
  spaces
  posstr <- many1 digit
  let pos = read posstr :: Int
  return $ SetY pos

setXY = do
  (try $ string "setxy") <|> string "xy"
  spaces
  xstr <- many1 digit
  spaces
  ystr <- many1 digit
  let x = read xstr :: Int
  let y = read ystr :: Int
  return $ SetXY x y

setAngle = do
  (try $ string "setangle") <|> (try $ string "angle") <|> string "a"
  spaces
  anglestr <- many1 digit
  let angle = read anglestr :: Int
  return $ SetAngle angle

home = do
  string "home"
  return Home

penColor = do
  (try $ string "pencolor") <|> (try $ string "color") <|> string "c"
  spaces
  rstr <- many1 digit
  spaces
  gstr <- many1 digit
  spaces
  bstr <- many1 digit
  spaces
  astr <- many1 digit
  let r = read rstr :: Int
  let g = read gstr :: Int
  let b = read bstr :: Int
  let a = read astr :: Int
  return $ PenColor r g b a
  
showTurtle = do
  (try $ string "showturtle") <|> (try $ string "show") <|> string "st"
  return ShowTurtle

hideTurtle = do
  (try $ string "hideturtle") <|> (try $ string "hide") <|> string "ht"
  return HideTurtle

clear = do
  string "clear"
  return Clear

turtleRepeat = do
  string "repeat"
  return $ Forward 10
  spaces
  repstr <- many1 digit
  spaces
  char '['
  exprs <- turtleSeq
  char ']'
  let rep = read repstr :: Int
  case exprs of
    Seq seqcmds -> return $ Repeat rep seqcmds
    expr -> return $ Repeat rep [expr]

-- preprocess text input before parsing
preprocessInput :: String -> String
preprocessInput input = foldr (\f acc -> f acc) input processors
  where processors = [map toLower, strip]

parseTurtle :: String -> Either ParseError TurtleExpr
parseTurtle input = parse turtleSeq "" (preprocessInput input)
--}

-- TYPE CHECKER
data TurtleType = CmdType | NumType | BoolType

instance Show TurtleType where
  show CmdType = "command"
  show NumType = "number"
  show BoolType = "boolean"

-- helper functions
isNumType :: TurtleType -> Bool
isNumType NumType = True
isNumType _       = False

isCmdType :: TurtleType -> Bool
isCmdType CmdType = True
isCmdType _       = False

isBoolType :: TurtleType -> Bool
isBoolType BoolType = True
isBoolType _        = False

-- generic function that typechecks
-- commands that take in integer arguments
validArgTypes :: (TurtleType -> Bool) -> TurtleType -> String -> [TurtleExpr] -> Either String TurtleType
validArgTypes validArg validType errorMsg args = do
  argTypes <- forM args typeTurtle
  if all validArg argTypes
    then Right validType
    else Left errorMsg

-- typecheck a single expression
typeTurtle :: TurtleExpr -> Either String TurtleType
typeTurtle expr = case expr of
  Forward steps ->
    validArgTypes isNumType CmdType "steps for forward cmd must be a number" [steps]

  Back steps ->
    validArgTypes isNumType CmdType "steps for back cmd must be a number" [steps]

  TurnRight angle ->
    validArgTypes isNumType CmdType "angle for turn right cmd must be a number" [angle]

  TurnLeft angle ->
    validArgTypes isNumType CmdType "angle for turn left cmd must be a number" [angle]

  DrawCircle radius ->
    validArgTypes isNumType CmdType "radius for circle cmd must be a number" [radius]

  PenUp ->
    Right CmdType

  PenDown ->
    Right CmdType

  SetX x ->
    validArgTypes isNumType CmdType "position for setx cmd must be a number" [x]

  SetY y ->
    validArgTypes isNumType CmdType "position for sety cmd must be a number" [y]

  SetXY x y ->
    validArgTypes isNumType CmdType "positions for setxy cmd must be numbers" [x,y]

  SetAngle angle ->
    validArgTypes isNumType CmdType "angle for setangle cmd must be a number" [angle]

  Home ->
    Right CmdType
  
  PenColor r g b a -> 
    validArgTypes isNumType CmdType "colors must be numbers" [r,g,b,a]

  ShowTurtle ->
    Right CmdType

  HideTurtle ->
    Right CmdType
  
  Clear ->
    Right CmdType

  Seq exprs -> do
    exprsTypes <- forM exprs typeTurtle
    if all isCmdType exprsTypes
      then Right CmdType
      else Left "sequence expressions must be commands"
    
  Repeat n exprs -> do
    nType <- typeTurtle n
    if isNumType nType
      then do
        exprsTypes <- forM exprs typeTurtle
        if all isCmdType exprsTypes
          then Right CmdType
          else Left "expressions in repeat list must be commands"
      else Left "iterations for repeat command must be a number"

  If pred thenExpr elseExpr -> do
    predType <- typeTurtle pred
    if isBoolType predType
      then do
        thenType <- typeTurtle thenExpr
        elseType <- typeTurtle elseExpr
        if isCmdType thenType && isCmdType elseType
          then Right CmdType
          else Left "conditional branches must be commands"
      else Left "predicate of if statement must be a boolean"
  
  Num _ ->
    Right NumType

  Add x y ->
    validArgTypes isNumType NumType "+ takes in two numbers" [x, y]

  Sub x y ->
    validArgTypes isNumType NumType "- takes in two numbers" [x, y]

  Mul x y ->
    validArgTypes isNumType NumType "* takes in two numbers" [x, y]

  BoolTrue ->
    Right BoolType
  
  BoolFalse ->
    Right BoolType

  And x y ->
    validArgTypes isBoolType BoolType "AND takes in two booleans" [x, y]

  Or x y ->
    validArgTypes isBoolType BoolType "OR takes in two booleans" [x, y]

  Not x ->
    validArgTypes isBoolType BoolType "NOT takes in a boolean" [x]

  Eq x y ->
    validArgTypes isNumType BoolType "== takes in two numbers" [x, y]
  
  Leq x y ->
    validArgTypes isNumType BoolType "<= takes in two numbers" [x, y]

  Geq x y ->
    validArgTypes isNumType BoolType ">= takes in two numbers" [x, y]

  Lt x y ->
    validArgTypes isNumType BoolType "< takes in two numbers" [x, y]

  Gt x y ->
    validArgTypes isNumType BoolType "> takes in two numbers" [x, y]
    
    

  

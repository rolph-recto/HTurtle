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

data TurtleExpr =
    Forward TurtleExpr
  | Back TurtleExpr
  | TurnRight TurtleExpr
  | TurnLeft TurtleExpr
  | DrawCircle TurtleExpr
  | PenUp
  | PenDown
  | SetX TurtleExpr
  | SetY TurtleExpr
  | SetXY TurtleExpr TurtleExpr
  | SetAngle TurtleExpr
  | Home
  | PenColor TurtleExpr TurtleExpr TurtleExpr TurtleExpr
  | ShowTurtle
  | HideTurtle
  | Clear
  -- ARITHMETIC EXPRS
  | Num Int                   
  | Add TurtleExpr TurtleExpr 
  | Sub TurtleExpr TurtleExpr
  | Mul TurtleExpr TurtleExpr
  | Div TurtleExpr TurtleExpr
  -- BOOLEAN EXPRS
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
  -- VARIABLES
  | Var String
  | SetVar String TurtleExpr
  -- FUNCTIONS AND OTHER
  | Seq [TurtleExpr]
  | Repeat TurtleExpr [TurtleExpr]
  | If TurtleExpr TurtleExpr TurtleExpr
  | Return TurtleExpr
  deriving (Show)

-- values that turtle expressions can evaluate to
data TurtleValue =
    TurtleNum Int
  | TurtleBool Bool
  | TurtleUnit
  -- a function and a list of its arguments (w/ names they bind to)
  | TurtleFunc [String]

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
-- variables must have their own type since we
-- assume variables can be any value;
-- thus VarTypes essentially get treated as wildcards
-- of course we're just making compile time errors to be
-- runtime errors; this could be a fixme in the future
data TurtleType = CmdType | NumType | BoolType | VarType

instance Show TurtleType where
  show CmdType = "command"
  show NumType = "number"
  show BoolType = "boolean"

-- helper functions
isNumType :: TurtleType -> Bool
isNumType NumType = True
isNumType VarType = True
isNumType _       = False

isCmdType :: TurtleType -> Bool
isCmdType CmdType = True
isCmdType VarType = True
isCmdType _       = False

isBoolType :: TurtleType -> Bool
isBoolType BoolType = True
isBoolType VarType  = True
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
  Forward steps -> do
    validArgTypes isNumType CmdType "steps for forward cmd must be a number" [steps]

  Back steps -> do
    validArgTypes isNumType CmdType "steps for back cmd must be a number" [steps]

  TurnRight angle -> do
    validArgTypes isNumType CmdType "angle for turn right cmd must be a number" [angle]

  TurnLeft angle -> do
    validArgTypes isNumType CmdType "angle for turn left cmd must be a number" [angle]

  DrawCircle radius -> do
    validArgTypes isNumType CmdType "radius for circle cmd must be a number" [radius]

  PenUp -> do
    return CmdType

  PenDown -> do
    return CmdType

  SetX x -> do
    validArgTypes isNumType CmdType "position for setx cmd must be a number" [x]

  SetY y -> do
    validArgTypes isNumType CmdType "position for sety cmd must be a number" [y]

  SetXY x y -> do
    validArgTypes isNumType CmdType "positions for setxy cmd must be numbers" [x,y]

  SetAngle angle -> do
    validArgTypes isNumType CmdType "angle for setangle cmd must be a number" [angle]

  Home -> do
    return CmdType
  
  PenColor r g b a -> do
    validArgTypes isNumType CmdType "colors must be numbers" [r,g,b,a]

  ShowTurtle -> do
    return CmdType

  HideTurtle -> do
    return CmdType
  
  Clear ->
    return CmdType

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

  Return retExpr -> do
    -- force type checking on return expr
    -- return doesn't really care what type it is
    retType <- typeTurtle retExpr
    return CmdType
  
  Num _ -> do
    return NumType

  Add x y -> do
    validArgTypes isNumType NumType "+ takes in two numbers" [x, y]

  Sub x y -> do
    validArgTypes isNumType NumType "- takes in two numbers" [x, y]

  Mul x y -> do
    validArgTypes isNumType NumType "* takes in two numbers" [x, y]

  BoolTrue -> do
    return BoolType
  
  BoolFalse -> do
    return BoolType

  And x y -> do
    validArgTypes isBoolType BoolType "AND takes in two booleans" [x, y]

  Or x y -> do
    validArgTypes isBoolType BoolType "OR takes in two booleans" [x, y]

  Not x -> do
    validArgTypes isBoolType BoolType "NOT takes in a boolean" [x]

  Eq x y -> do
    validArgTypes isNumType BoolType "== takes in two numbers" [x, y]
  
  Leq x y -> do
    validArgTypes isNumType BoolType "<= takes in two numbers" [x, y]

  Geq x y -> do
    validArgTypes isNumType BoolType ">= takes in two numbers" [x, y]

  Lt x y -> do
    validArgTypes isNumType BoolType "< takes in two numbers" [x, y]

  Gt x y -> do
    validArgTypes isNumType BoolType "> takes in two numbers" [x, y]
    
  Var _ -> do
    return VarType
    

  

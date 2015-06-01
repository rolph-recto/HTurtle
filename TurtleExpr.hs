-- TurtleExpr
-- definition and parser for turtle command language

module TurtleExpr (
  TurtleExpr(..),
  parseTurtle
) where

import Data.Char
import Data.String.Utils
import Debug.Trace
import Text.ParserCombinators.Parsec

data TurtleExpr = Forward Int               -- move turtle forward
                | Back Int                  -- move turtle backward
                | TurnRight Int             -- rotate turtle rightward
                | TurnLeft Int              -- rotate turtle leftward
                | DrawCircle Int            -- draw a circle of some radius
                | PenUp                     -- turtle doesn't draw
                | PenDown                   -- turtle draws again
                | SetX Int                  -- set turtle x-coord
                | SetY Int                  -- set turtle y-coord
                | SetXY Int Int             -- set turtle coords
                | SetAngle Int              -- set turtle angle
                | Home                      -- move turtle to (0,0)
                | PenColor Int Int Int Int  -- set pen color
                | ShowTurtle                -- show the turtle
                | HideTurtle                -- hide the turtle
                | Clear                     -- clear the screen
                | Seq [TurtleExpr]          -- sequence of commands
                | Repeat Int [TurtleExpr]   -- repeat a sequence of commands
                deriving (Show)

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
          <|> turtleRepeat

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

-- TurtleExpr
-- definition and parser for turtle command language

module TurtleExpr (
  TurtleExpr(..)
) where

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


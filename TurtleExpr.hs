-- TurtleExpr
-- definition and parser for turtle command language

module TurtleExpr (
  TurtleExpr(..)
) where

data TurtleExpr = Forward Int             -- move turtle forward
                | Back Int                -- move turtle backward
                | TurnRight Int           -- rotate turtle rightward
                | TurnLeft Int            -- rotate turtle leftward
                | Circle Int              -- draw a circle of some radius
                | PenUp                   -- turtle doesn't draw
                | PenDown                 -- turtle draws again
                | SetX Int                -- set turtle x-coord
                | SetY Int                -- set turtle y-coord
                | SetXY Int               -- set turtle coords
                | Home                    -- move turtle to (0,0)
                | Color Int Int Int Int   -- set pen color
                | Clear                   -- clear the screen
                | Repeat Int [TurtleExpr] -- repeat a list of commands

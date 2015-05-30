module TurtleState (
  TurtleState(..)
, CanvasShape(..)
) where

import Graphics.Gloss.Interface.IO.Game hiding (color)

-- stuff that can be draw to the screen
data CanvasShape = CanvasLine {
                    x1 :: Float
                  , y1 :: Float
                  , x2 :: Float
                  , y2 :: Float
                  , color :: Color
                  }
                | CanvasCircle {
                  rad :: Float -- radius
                , cx :: Float -- x coord of center
                , cy :: Float -- y coord of center
                , color :: Color
                }
                deriving (Show)

-- state of the turtle and screen
data TurtleState = TurtleState {
                    tx :: Float -- turtle x coord
                  , ty :: Float -- turtle y coord
                  , tsize :: Float -- turtle size
                  , tangle :: Float -- turtle angle (0 is straight up)
                  , tshow :: Bool -- draw the turtle
                  , pen :: Bool
                  , penColor :: Color
                  , shapes :: [CanvasShape]
                  }
                  deriving (Show)

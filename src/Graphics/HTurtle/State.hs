module Graphics.HTurtle.State (
  DrawState(..), CanvasShape(..),
  ZoomPress(..), TurtleState
) where

import Graphics.Gloss.Interface.IO.Game hiding (color)

import Language.HLisp.Expr

data ZoomPress = NoZoom | ZoomIn | ZoomOut deriving (Show)

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
                | CanvasArc {
                  rad :: Float
                , cx :: Float
                , cy :: Float
                , arcStart :: Float -- initial angle
                , arcEnd :: Float  -- terminal angle
                , arcRot :: Float
                , color :: Color
                }
                deriving (Show)

-- state of the turtle and screen
data DrawState = DrawState {
                    tx :: Float -- turtle x coord
                  , ty :: Float -- turtle y coord
                  , tsize :: Float -- turtle size
                  , tangle :: Float -- turtle angle (0 is straight up)
                  , tshow :: Bool -- draw the turtle
                  , pen :: Bool
                  , penColor :: Color
                  , shapes :: [CanvasShape]
                  , zoom :: Float -- zoom factor of canvas
                  , camx :: Float -- camera position
                  , camy :: Float
                  -- camActive and zoomActive should be mutually exclusive
                  , camActive :: Bool -- is mouse translating camera?
                  , zoomActive :: Bool -- is mouse zooming camera?
                  , zoomPress :: ZoomPress -- is zoom key being pressed?
                  , lastx :: Float
                  , lasty :: Float
                  }
                  deriving (Show)

type TurtleState = LispState DrawState

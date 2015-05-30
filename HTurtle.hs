import System.IO
import Control.Monad.State
import Graphics.Gloss.Interface.IO.Game hiding (color)

import TurtleState
import TurtleExpr
import TurtleCmd
import TurtleDraw


runCmd :: TurtleState -> TurtleExpr -> IO TurtleState
runCmd tstate cmd  = do
  let (log, tstate') = runState (execTurtleCmd cmd) tstate
  putStrLn log
  return tstate'

-- handle user input
handleEvents :: Event -> TurtleState -> IO TurtleState
handleEvents (EventKey (SpecialKey KeyUp) Down _ _) tstate = do
  let cmd = Forward 10
  tstate' <- runCmd tstate cmd
  return tstate'

handleEvents (EventKey (SpecialKey KeyDown) Down _ _) tstate = do
  let cmd = Back 10
  tstate' <- runCmd tstate cmd
  return tstate'

handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) tstate = do
  let cmd = TurnLeft 10
  tstate' <- runCmd tstate cmd
  return tstate'

handleEvents (EventKey (SpecialKey KeyRight) Down _ _) tstate = do
  let cmd = TurnRight 10
  tstate' <- runCmd tstate cmd
  return tstate'

handleEvents _ tstate = do
  return tstate


-- step logo
stepLogo :: Float -> TurtleState -> IO TurtleState
stepLogo _ tstate = do
  return tstate

{--
  putStr "? "
  cmdstr <- getLine
  putStrLn $ "lol i don't know how to " ++ cmdstr ++ " yet"
  let cmd = Forward 10 -- dummy command for now
  let (log, tstate') = runState (execTurtleCmd cmd) tstate
  return tstate'
--}


-- initial config
initTurtleState = TurtleState {
                  tx = 0
                , ty = 0
                , tsize = 20
                , tangle = 0
                , pen = True
                , penColor = white
                , shapes = []
                }

main = do
  putStrLn "HTurtle v0.1 by Rolph Recto"
  playIO
    (InWindow "HTurtle" (600, 600) (100, 100))
    black         -- background color
    30            -- simulation steps per minute
    initTurtleState
    drawTurtleState
    handleEvents
    stepLogo

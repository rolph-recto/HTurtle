import System.IO
import System.Random
import Control.Monad.State
import Control.Concurrent
import Data.Tuple.Select
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
handleEvents event tstate = case event of
  EventKey (SpecialKey KeyUp) Down _ _ -> do
    let cmd = Forward 10
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (SpecialKey KeyDown) Down _ _ -> do
    let cmd = Back 10
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (SpecialKey KeyLeft) Down _ _ -> do
    let cmd = TurnLeft 10
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (SpecialKey KeyRight) Down _ _ -> do
    let cmd = TurnRight 10
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char 'c') Down _ _ -> do
    let cmd = DrawCircle 50
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char '1') Down _ _ -> do
    let cmd = Repeat 36 [Repeat 6 [PenColor 150 150 150 255, DrawCircle 100, PenColor 255 255 255 255, Forward 50, TurnRight 60], TurnRight 10]
    let cmd2 = Repeat 36 [Repeat 6 [PenColor 100 100 100 255, DrawCircle 150, PenColor 50 50 50 255, Forward 100, TurnRight 60], TurnRight 10]
    tstate' <- runCmd tstate $ Seq [cmd, cmd2]
    return tstate'

  EventKey (Char '2') Down _ _ -> do
    let cmd = Seq [Seq [PenColor 255 255 255 255, Repeat 4 [TurnRight 85, Forward (5*i)]] | i <- [1..75]]
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char '3') Down _ _ -> do
    let cmd = Seq [Seq [PenColor (255-i*2) (255-i*2) (255-i*2) 255, Repeat 3 [TurnRight 110, Forward (5*i)]] | i <- [1..100]]
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char '4') Down _ _ -> do
    ccycle <- sequence $ take 4 $ repeat $ do
      r <- getStdRandom $ randomR (0,255)
      g <- getStdRandom $ randomR (0,255)
      b <- getStdRandom $ randomR (0,255)
      return (r,g,b)
    let colors = take 201 $ cycle ccycle
    let cmd = Seq [Seq [PenColor (sel1 (colors!!i)) (sel2 (colors!!i)) (sel3 (colors!!i)) 255, TurnRight 89, Forward (2*i)] | i <- [1..200]]
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char 'q') Down _ _ -> do
    let cmd = Seq [Clear, Home, PenColor 255 255 255 255]
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char 's') Down _ _ -> do
    if tshow tstate
    then do
      tstate' <- runCmd tstate HideTurtle
      return tstate'
    else do
      tstate' <- runCmd tstate ShowTurtle
      return tstate'

  EventKey (SpecialKey KeySpace) Down _ _ -> do
    if pen tstate
    then do
      tstate' <- runCmd tstate PenUp
      return tstate'
    else do
      tstate' <- runCmd tstate PenDown
      return tstate'


  _ -> do
    return tstate


-- step logo
stepLogo :: Float -> TurtleState -> IO TurtleState
stepLogo _ tstate = do
  return tstate

{--
stepLogo :: Float -> TurtleState -> IO TurtleState
stepLogo _ tstate = do
  putStr "? "
  eof <- isEOF
  if not eof
  then do
    cmdstr <- getLine
    putStrLn $ "lol i don't know how to " ++ cmdstr ++ " yet"
    let cmd = Forward 10 -- dummy command for now
    let (log, tstate') = runState (execTurtleCmd cmd) tstate
    return tstate'
  else do
    return tstate
--}


-- initial config
initTurtleState = TurtleState {
                  tx = 0
                , ty = 0
                , tshow = True
                , tsize = 20
                , tangle = 0
                , pen = True
                , penColor = white
                , shapes = []
                }

main = do
  -- adjust IO settings
  hSetBuffering stdin LineBuffering

  putStrLn "HTurtle v0.1 by Rolph Recto"
  playIO
    (InWindow "HTurtle" (600, 600) (100, 100))
    black         -- background color
    30            -- simulation steps per minute
    initTurtleState
    drawTurtleState
    handleEvents
    stepLogo

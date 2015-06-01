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
runCmd tstate cmd = do
  let (log, tstate') = runState (execTurtleCmd cmd) tstate
  putStrLn log
  return tstate'

runCmdStr :: TurtleState -> String -> IO TurtleState
runCmdStr tstate cmdstr = do
  let parseResult = parseTurtle cmdstr
  case parseResult of
    Left parseErr -> do
      print parseErr
      return tstate
    Right cmd -> do
      let (log, tstate') = runState (execTurtleCmd cmd) tstate
      putStrLn log
      return tstate'

-- handle user input
handleEvents :: Event -> TurtleState -> IO TurtleState
handleEvents event tstate = case event of
  EventKey (SpecialKey KeyUp) Down _ _ -> do
    let cmd = "fd 10"
    tstate' <- runCmdStr tstate cmd
    return tstate'

  EventKey (SpecialKey KeyDown) Down _ _ -> do
    let cmd = "bk 10"
    tstate' <- runCmdStr tstate cmd
    return tstate'

  EventKey (SpecialKey KeyLeft) Down _ _ -> do
    let cmd = "left 10"
    tstate' <- runCmdStr tstate cmd
    return tstate'

  EventKey (SpecialKey KeyRight) Down _ _ -> do
    let cmd = "right 10"
    tstate' <- runCmdStr tstate cmd
    return tstate'

  EventKey (Char '1') Down _ _ -> do
    let cmd = "repeat 36 [repeat 6 [color 150 150 150 255 circle 100 color 255 255 255 255 fd 50 right 60] right 10]"
    let cmd2 = "repeat 36 [repeat 6 [color 100 100 100 255 circle 150 color 50 50 50 255 fd 100 right 60] right 10]"
    tstate' <- runCmdStr tstate $ cmd ++ " " ++ cmd2
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
    let cmd = "clear home color 255 255 255 255"
    tstate' <- runCmdStr tstate cmd
    return tstate'

  EventKey (Char 's') Down _ _ -> do
    if tshow tstate
    then do
      tstate' <- runCmdStr tstate "hide"
      return tstate'
    else do
      tstate' <- runCmdStr tstate "show"
      return tstate'

  EventKey (SpecialKey KeySpace) Down _ _ -> do
    if pen tstate
    then do
      tstate' <- runCmdStr tstate "up"
      return tstate'
    else do
      tstate' <- runCmdStr tstate "down"
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
  cmdstr <- forkIO getLine
  putStrLn $ "lol i don't know how to " ++ cmdstr ++ " yet"
  let cmd = Forward 10 -- dummy command for now
  let (log, tstate') = runState (execTurtleCmd cmd) tstate
  return tstate'
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

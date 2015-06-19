import System.IO
import System.Random
import Control.Monad.State
import Data.Tuple.Select
import Data.String.Utils
import EitherT

import Graphics.Gloss.Interface.IO.Game hiding (color)

import TurtleState
import TurtleExpr
import TurtleCmd
import TurtleDraw

runCmd :: TurtleState -> TurtleExpr -> IO TurtleState
runCmd tstate cmd = do
  -- type check the expression before executing it
  case typeTurtle cmd of
    Left err -> do
      print err
      return tstate
    Right _ -> do
      let (log, tstate') = runState (runEitherT (execTurtleCmd cmd)) tstate
      case log of
        Left err2 -> do
          print err2
          -- returning tstate instead of tstate' ensures that if the expr
          -- causes a crash that the TurtleState is kept the same as
          -- before the execution of the expression
          -- alternatively, we can also return tstate', which will keep
          -- the changes made to the TurtleState while executing the expr
          -- prior to the crash
          return tstate
        Right _ -> do
          return tstate'

{-
runCmdStr :: TurtleState -> String -> IO TurtleState
runCmdStr tstate cmdstr = do
  let parseResult = parseTurtle cmdstr
  case parseResult of
    Left parseErr -> do
      print parseErr
      return tstate
    Right cmd -> do
      let (log, tstate') = runState (execTurtleCmd cmd) tstate
      let log' = strip log
      if length log' > 0 then putStrLn log' else return ()
      return tstate'
-}

-- handle user input
handleEvents :: Event -> TurtleState -> IO TurtleState
handleEvents event tstate = case event of
  EventKey (SpecialKey KeyUp) Down _ _ -> do
    let cmd = Forward (Num 10)
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (SpecialKey KeyDown) Down _ _ -> do
    let cmd = Back (Num 10)
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (SpecialKey KeyLeft) Down _ _ -> do
    let cmd = TurnLeft (Num 10)
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (SpecialKey KeyRight) Down _ _ -> do
    let cmd = TurnRight (Num 10)
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char '1') Down _ _ -> do
    let cmd = Repeat (Num 36) [Repeat (Num 6) [PenColor (Num 150) (Num 150) (Num 150) (Num 255), DrawCircle (Num 100), PenColor (Num 255) (Num 255) (Num 255) (Num 255), Forward (Num 50), TurnRight (Num 60)], TurnRight (Num 10)]
    let cmd2 = Repeat (Num 36) [Repeat (Num 6) [PenColor (Num 100) (Num 100) (Num 100) (Num 255), DrawCircle (Num 150), PenColor (Num 50) (Num 50) (Num 50) (Num 255), Forward (Num 100), TurnRight (Num 60)], TurnRight (Num 10)]
    tstate' <- runCmd tstate $ Seq [cmd, cmd2]
    return tstate'

  EventKey (Char '2') Down _ _ -> do
    let cmd = Seq [Seq [PenColor (Num 255) (Num 255) (Num 255) (Num 255), Repeat (Num 4) [TurnRight (Num 85), Forward (Num (5*i))]] | i <- [1..75]]
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char '3') Down _ _ -> do
    let cmd = Seq [Seq [PenColor (Num (255-i*2)) (Num (255-i*2)) (Num (255-i*2)) (Num 255), Repeat (Num 3) [TurnRight (Num 110), Forward (Num (5*i))]] | i <- [1..100]]
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char '4') Down _ _ -> do
    ccycle <- sequence $ take 4 $ repeat $ do
      r <- getStdRandom $ randomR (0,255)
      g <- getStdRandom $ randomR (0,255)
      b <- getStdRandom $ randomR (0,255)
      return (r,g,b)
    let colors = take 201 $ cycle ccycle
    let cmd = Seq [Seq [PenColor (Num $ sel1 (colors!!i)) (Num $ sel2 (colors!!i)) (Num $ sel3 (colors!!i)) (Num 255), TurnRight (Num 89), Forward (Num (2*i))] | i <- [1..200]]
    tstate' <- runCmd tstate cmd
    return tstate'

  EventKey (Char 'q') Down _ _ -> do
    let cmd = Seq [Clear, Home, PenColor (Num 255) (Num 255) (Num 255) (Num 255)]
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


fetchReplCmd :: IO (Maybe String)
fetchReplCmd = do
  ready <- hReady stdin
  if ready
    then do
      cmdstr <- getLine
      return $ Just cmdstr
    else return Nothing

-- step logo
stepLogo :: Float -> TurtleState -> IO TurtleState
stepLogo _ tstate = do
  replcmd <- fetchReplCmd
  case replcmd of
    -- need to implement parser first...
    -- Just cmdstr -> runCmdStr tstate cmdstr
    Just cmdstr -> return tstate
    Nothing -> return tstate

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

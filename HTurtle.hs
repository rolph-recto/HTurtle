import System.IO
import System.Random
import System.Directory (doesFileExist)

import Control.Monad.State

import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.Tuple.Select
-- import Data.String.Utils

import Graphics.Gloss.Interface.IO.Game hiding (color)

import HLispExpr
import HLispParse
import HLispEval
import HLispPrim

import TurtleState
import TurtleDraw
import TurtleCommand
import LSystem

-- settings
windowWidth = 600 :: Int
windowHeight = 600 :: Int
zoomSpeed = 0.1

-- sierpisnki triangle commands
-- generate lisp commands

cmdA :: State () String
cmdA = return "[fd 20]"

cmdB :: State () String
cmdB = return "[fd 20]"

cmdPlus :: State () String
cmdPlus = return "[left 60]"

cmdMinus :: State () String
cmdMinus = return "[right 60]"

-- handle user input
handleEvents :: Event -> TurtleState -> IO TurtleState
handleEvents event tstate@(dstate,env) = case event of
  EventKey (SpecialKey KeyUp) _ _ _ -> do
    tstate' <- runCmd tstate "[fd 10]"
    return tstate'

  EventKey (SpecialKey KeyDown) _ _ _ -> do
    tstate' <- runCmd tstate "[back 10]"
    return tstate'

  EventKey (SpecialKey KeyLeft) _ _ _ -> do
    tstate' <- runCmd tstate "[lt 10]"
    return tstate'

  EventKey (SpecialKey KeyRight) _ _ _ -> do
    tstate' <- runCmd tstate "[rt 10]"
    return tstate'

  EventKey (Char 'q') Down _ _ -> do
    tstate' <- runCmd tstate "[[reset] [color 255 255 255 255]]"
    return tstate'

  EventKey (Char 's') Down _ _ -> do
    if tshow dstate
    then do
      tstate' <- runCmd tstate "[hide]"
      return tstate'
    else do
      tstate' <- runCmd tstate "[show]"
      return tstate'

  EventKey (Char 't') Down _ _ -> do
    let a = S 'A'
    let b = S 'B'
    let plus = S '+'
    let minus = S '-'
    let rules = [(a,[plus,b,minus,a,minus,b,plus]),(b,[minus,a,plus,b,plus,a,minus])]
    let sierpinski = LS { axiom = [a], rules = (M.fromList rules) }
    let cmds = M.fromList [(a,cmdA), (b,cmdB), (plus,cmdPlus), (minus,cmdMinus)]
    let drawcmds = evalState (evalLSystem 8 cmds sierpinski) ()
    foldM runCmd tstate drawcmds

  EventKey (SpecialKey KeySpace) Down _ _ -> do
    if pen dstate
    then do
      tstate' <- runCmd tstate "[penup]"
      return tstate'
    else do
      tstate' <- runCmd tstate "[pendown]"
      return tstate'

  EventKey (Char 'z') Down _ _ -> do
    let z = zoom dstate
    let z' = if z - zoomSpeed < 0.0 then 0.0 else z - zoomSpeed
    return (dstate { zoom = z' }, env)

  EventKey (Char 'x') Down _ _ -> do
    let z = zoom dstate
    let z' = z + zoomSpeed
    return (dstate { zoom = z' }, env)

  EventKey (MouseButton LeftButton) Down _ (mx,my) -> do
    return (dstate { camActive = True, lastx = mx, lasty = my}, env)

  EventKey (MouseButton LeftButton) Up _ _ -> do
    return (dstate { camActive = False }, env)

  EventKey (MouseButton RightButton) Down _ (mx,my) -> do
    return (dstate { zoomActive = True, lastx = mx, lasty = my}, env)

  EventKey (MouseButton RightButton) Up _ _ -> do
    return (dstate { zoomActive = False }, env)

  EventMotion (mx,my) -> do
    if camActive dstate
    then do
      let dx = mx - (lastx dstate)
      let dy = my - (lasty dstate)
      let camx' = (camx dstate) - dx
      let camy' = (camy dstate) - dy
      let dstate' = dstate { camx = camx', camy = camy', lastx = mx, lasty = my }
      return (dstate', env)
    else if zoomActive dstate
      then do
      let (x,y) = (lastx dstate, lasty dstate)
      -- distance from origin
      let r = sqrt (x*x + y*y)
      let r' = sqrt (mx*mx + my*my)
      let dz = (r-r') / ((fromIntegral windowWidth)/4.0)
      let z = if (zoom dstate) - dz < 0.0 then 0.0 else (zoom dstate) - dz
      return (dstate { zoom = z }, env)
      else return tstate

  _ -> do
    return tstate

runCmd :: TurtleState -> String -> IO TurtleState
runCmd tstate@(dstate,env) cmdstr = do
  case words cmdstr of
    -- repl commands
    [] -> return tstate
    
    ("globals":_) -> do
      let 
      let userGlobals = filter isUserGlobal $ M.toList env
      if length userGlobals > 0
      then do
        let userBinds = map fst userGlobals
        putStr "User-defined globals: "
        putStrLn $ intercalate " " userBinds
        return tstate

      else do
        putStrLn "No user-defined globals."
        return tstate

    ("clearglobals":_) -> do
      let env' = M.fromList $ filter (not . isUserGlobal) $ M.toList env
      putStrLn "Removed all user-defined globals."
      return (dstate, env')

    ("info":bind:_) -> do
      case M.lookup bind env of
        Just val -> do
          putStrLn $ bind ++ " : " ++ (show val)
          return tstate

        Nothing -> do
          putStrLn $ "No binding found for " ++ bind ++ "."
          return tstate

    ("load":file:_) -> do
      fileExists <- doesFileExist file
      if fileExists
      then do
        withFile file ReadMode $ \h -> do
          filestr <- hGetContents h
          case parseLispFile filestr of
            Left err -> do
              putStrLn $ show err
              return tstate
          
            Right exprs -> do
              let exprList = LispList exprs
              (result, tstate') <- runLisp tstate exprList
              case result of
                Left err -> do
                  putStrLn err
                  return tstate'

                Right _ -> do
                  return tstate'

        else do
          putStrLn "File doesn't exist!"
          return tstate

    -- a lisp command
    otherwise -> do
      let parseResult = parseLisp cmdstr
      case parseResult of
        Left parseErr -> do
          print parseErr
          return tstate

        Right expr -> do
          (result, tstate') <- runLisp tstate expr
          case result of
            Left err -> do
              putStrLn err
              return tstate'

            Right LispUnit -> do
              return tstate'

            Right val -> do
              putStrLn (show val)
              return tstate'

  where isPrim (LispPrimFunc _ _) = True
        isPrim _                  = False
        isUserGlobal (key,val)    = not $ isPrim val


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
    Just cmdstr -> runCmd tstate cmdstr
    Nothing -> return tstate

-- initial config
initTurtleState = (dstate, globalEnv)
  where globalEnv = registerPrimitives M.empty (primitives ++ turtleCommands)
        dstate = DrawState {
                  tx = 0
                , ty = 0
                , tshow = True
                , tsize = 20
                , tangle = 0
                , pen = True
                , penColor = white
                , shapes = []
                , zoom = 1.0
                , camx = 0
                , camy = 0
                , camActive = False
                , zoomActive = False
                , lastx = 0.0
                , lasty = 0.0
                }

main = do
  -- adjust IO settings
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering

  putStrLn "HTurtle v0.1 by Rolph Recto"
  playIO
    (InWindow "HTurtle" (windowWidth, windowHeight) (0, 0))
    black         -- background color
    30            -- simulation steps per minute
    initTurtleState
    drawTurtleState
    handleEvents
    stepLogo

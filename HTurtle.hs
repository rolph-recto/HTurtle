import System.IO
import System.Random
import System.Directory (doesFileExist)

import Control.Monad.State

import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.Tuple.Select
import Data.String.Utils

import Graphics.Gloss.Interface.IO.Game hiding (color)

import HLispExpr
import HLispParse
import HLispEval
import HLispPrim

import TurtleState
import TurtleDraw
import TurtleCommand

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

  EventKey (SpecialKey KeySpace) Down _ _ -> do
    if pen dstate
    then do
      tstate' <- runCmd tstate "[penup]"
      return tstate'
    else do
      tstate' <- runCmd tstate "[pendown]"
      return tstate'

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
                }

main = do
  -- adjust IO settings
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering

  putStrLn "HTurtle v0.1 by Rolph Recto"
  playIO
    (InWindow "HTurtle" (600, 600) (100, 100))
    black         -- background color
    30            -- simulation steps per minute
    initTurtleState
    drawTurtleState
    handleEvents
    stepLogo

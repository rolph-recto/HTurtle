module Graphics.HTurtle.LSystem (Symbol(..), LSystem(..), evalLSystem) where

import Control.Monad
import Control.Monad.State
import Data.Map.Strict as M

type Symbol = Char
type Term = [Symbol]
data LSystem s a = LS { axiom :: Term, rules :: M.Map Symbol Term }

type Commands s a = M.Map Symbol (State s a)

evalLSystem :: Int -> Commands s a -> s -> LSystem s a -> (Term, [a])
evalLSystem n cmds s lsys =
  let term = eval n (rules lsys) (axiom lsys) in
  let result = evalState (st term) s in
  (term, result)
  where st t              = runCmds cmds t 
        eval 0  _ term    = term
        eval m rules term =
          let term' = concatMap (\s -> maybe [s] id (M.lookup s rules)) term in
          eval (m-1) rules term'
        runCmd cmds acc s = do
          let mcmd = M.lookup s cmds
          case mcmd of
            Nothing -> return acc
            Just cmd -> do
              r <- cmd
              return (r:acc)
        runCmds cmds term = foldM (runCmd cmds) [] term


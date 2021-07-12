{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Repl where

import Game
import Parse
import Render
import Move

import System.Console.Repline
import Control.Monad.State
import Control.Monad.Except
import System.Exit (exitSuccess)
import Data.Either (fromRight)

type Repl a = HaskelineT (StateT Game IO) a

hoistError :: Either String a -> Repl a
hoistError (Left err) = do
    liftIO $ putStrLn err
    abort
hoistError (Right a) = return a

----------------------------------------
-- execution
----------------------------------------

printGame :: Repl ()
printGame = do
    g <- get
    liftIO (putStrLn (renderGame g))

exec :: String -> Repl ()
exec source = do
    getGameState >>= \case
      InProgress -> return ()
      Won -> liftIO $ do
          putStrLn "You won!"
          exitSuccess 
      Lost -> liftIO $ do
          putStrLn "You lost :("
          exitSuccess
    move <- hoistError (parseMove source)
    g <- get
    put =<< hoistError (runMove g (performMove move))
    printGame

----------------------------------------
-- commands
----------------------------------------

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

help :: a -> Repl ()
help _ = liftIO $ putStrLn helpText

helpText = "" ++ unlines [name ++ "\t" ++ msg | (name, _, msg) <- commands]

----------------------------------------
-- Interactive shell
----------------------------------------

-- TODO do the commands here so they show up on help
commands :: [([Char], a -> Repl (), [Char])]
commands =
  [ ("quit", quit, "exit the repl"),
    ("help", help, "display this help text")
  ]

opts :: [(String, String -> Repl ())]
opts = [(name, f) | (name, f, _) <- commands]

----------------------------------------
-- Entry Point
----------------------------------------

shell :: Repl a -> IO ()
shell pre = do
    deck <- shuffledDeck
    flip evalStateT (fromRight emptyGame $ makeGame deck) $
        evalRepl (const $ pure "Solitaire> ") exec opts (Just ':') Nothing File pre (pure Exit)

run :: IO ()
run = shell printGame

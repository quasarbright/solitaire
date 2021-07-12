{-# LANGUAGE FlexibleContexts #-}
module Repl where

import Game
import Parse
import Render
import Move

import System.Console.Repline
import Control.Monad.State
import Control.Monad.Except

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
    move <- hoistError (parseMove source)
    g <- get
    put =<< hoistError (runMove g (performMove move))
    printGame

----------------------------------------
-- commands
----------------------------------------



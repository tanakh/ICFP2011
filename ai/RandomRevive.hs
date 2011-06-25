#!/usr/bin/env runhaskell
import LTG

import Control.Monad
import Control.Monad.State
import System.Random

main :: IO ()
main = do 
  setStdGen $ mkStdGen 0x9b
  runLTG $ forever $ do
    s  <- liftIO $ randomRIO (0, 10::Int)
    if s==0 
    then do
      ensureAlive 0
      nop
    else do
      nop

    

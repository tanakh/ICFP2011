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
    if s==0 || True
    then do
      --ensureAlive 0
      --revive 0
      Put $> 1
      1 $< Zero
      Revive $> 1
      t <- getTurnCnt
      v <- getVital True 0 
      lprint $ "v t = " ++ show v ++ " " ++ show t
      --nop
    else do
      nop

    

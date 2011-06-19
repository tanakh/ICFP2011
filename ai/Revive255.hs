#!/usr/bin/env runhaskell

import Control.Monad
import LTG
import Data.IORef
import System.IO.Unsafe


hima, busy :: IORef Int
hima = unsafePerformIO $ newIORef 0
busy = unsafePerformIO $ newIORef 0
{-# NOINLINE hima #-}
{-# NOINLINE busy #-}

main :: IO ()
main = runLTG $ do
         num 1 255
         num 0 1
         Get $> 0
         forever recover

recover :: LTG ()
recover = do
  a0 <- isAlive True 0
  a1 <- isAlive True 1
  a255 <- isAlive True 255
  [f0, f1] <- mapM (getField True) [0,1]
  if (a0 && a1 && f0 == VInt 255 && f1 == VInt 255) 
  then do 
    if a255 then nop
    else Revive $> 0
  else do 
    when (not a0) $ revive 0 >> return ()
    when (not a1) $ revive 1 >> return ()
    when (f0 /= VInt 255 && f1 == VInt 255) $ do 
            num 0 1
            Get $> 0
    when (f0 == VInt 255 && f1 /= VInt 255) $ do 
            num 1 0
            Get $> 1
  

  
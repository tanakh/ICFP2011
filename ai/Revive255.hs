#!/usr/bin/env runhaskell

import Control.Monad
import Control.Monad.State(get)
import Control.Monad.Trans
import LTG
import Data.IORef
import System.IO
import System.IO.Unsafe


hima :: IORef Int
hima = unsafePerformIO $ newIORef 0
{-# NOINLINE hima #-}

main :: IO ()
main = runLTG $ do
         num 1 255
         num 0 1
         Get $> 0
         forever $ do
                  s <- get
                  lift $ record (turnCnt s)
                  recover
                  
record :: Int -> IO ()
record t = do
  h <- readIORef hima
  when (t==99999)$  writeFile "Revive255.txt" $ show h ++ "/" ++ show t 

recover :: LTG ()
recover = do
  a0 <- isAlive True 0
  a1 <- isAlive True 1
  a255 <- isAlive True 255
  [f0, f1] <- mapM (getField True) [0,1]
  if (a0 && a1 && f0 == VInt 255 && f1 == VInt 255) 
  then do 
    if a255 then do
              lift $ modifyIORef hima (+1)
              nop
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
  

  
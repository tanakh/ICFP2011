#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import Network.MessagePackRpc.Client
 
import Control.Concurrent
import Control.Monad
import League 

add :: RpcMethod (Int -> Int -> IO Int)
add  = method "add"
 
echo :: RpcMethod (String -> IO String)
echo = method "echo"
 
main :: IO ()
main = do
  conn <- connect "p01" port
  forM_ [0..100] $ \i -> do
    print =<< add conn i (i*2)

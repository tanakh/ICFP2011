#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import League
import Network.MessagePackRpc.Server
import System.Posix.Unistd

add :: Int -> Int -> IO Int
add x y = do
  _ <- sleep 1
  return $ x+y
 
echo :: String -> IO String
echo s = do
  _ <- sleep 1
  return s
 
main :: IO ()
main = do
  serve port [ ("add", fun add), ("echo", fun echo) ]

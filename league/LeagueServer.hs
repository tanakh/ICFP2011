#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import Control.Concurrent.STM
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import League
import Network.MessagePackRpc.Server
import System.IO.Unsafe
import System.Posix.Unistd
import System.Posix.Files



{-
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
-}

resultFile = "result.txt"


scoreBoard :: TVar (Vector (Vector Int))
scoreBoard = unsafePerformIO $ newTVarIO $ replicate aiSize $ replicate aiSize 0

recordScore :: Match -> IO ()
recordScore m = do
  let ai0 = p0 m
      ai1 = p1 m
      sco = score m
  

main :: IO ()
main = do
  results <- do
         exist <- fileExist resultFile
         if exist then fmap lines $ readFile resultFile
         else return []
  let matches :: [Match]
      matches = map read results
  mapM_ recordScore matches
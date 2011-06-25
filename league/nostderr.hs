#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import Control.Concurrent
import Control.Monad
import Data.Time
import System.Environment
import System.IO
import System.Process
import System.Timeout


trash :: Handle -> IO ()
trash hdl = forever $ hGetLine hdl >> return ()

getHands :: Handle -> IO ()
getHands procIn = do
  replicateM_ 3 $ hGetLine stdin >>= hPutStrLn procIn
  hFlush procIn
             

putHands :: Handle -> IO ()
putHands procOut = do
  tz <- getCurrentTimeZone 
  ut <- getCurrentTime

  hPutStrLn stderr $ "hajimea" ++ (show $ utcToLocalTime tz ut)
  replicateM_ 3 $ hGetLine procOut >>= hPutStrLn stdout
  hFlush stdout
  hPutStrLn stderr $ "owar"

play :: Int -> Handle -> Handle -> IO ()
play phase procIn procOut = do
  when (phase==1) $ getHands procIn
  forever $ do
    ret <- timeout (10^(7::Int)) $ putHands procOut
    case ret of
      Just () -> return ()
      Nothing -> hPutStrLn stdout "timeout"
    getHands procIn

main :: IO ()
main = do
  args <- getArgs
  let phase = (read $ last args)
  (Just procIn, Just procOut, Just procErr, hdl) <- createProcess 
    (shell $ unwords args) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  _ <- forkIO $ trash procErr
  play phase procIn procOut
  _ <- waitForProcess hdl
  return ()
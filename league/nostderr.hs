#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Time
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.Posix.Unistd (usleep)
import System.Process


turn :: TVar Int
turn = unsafePerformIO $ newTVarIO 0
{-# NOINLINE turn #-}

killer :: ProcessID -> Int -> Int -> IO ()
killer pid t usec = do
  usleep usec
  t' <- atomically $ readTVar turn
  if (t==t') 
  then do 
    _ <- system $ "kill " ++ show pid
    return ()
  else return ()

timeout :: Int -> IO a -> IO a
timeout usec m = do
  pid <- getProcessID
  t <- atomically $ readTVar turn
  forkIO $ killer pid t usec
  ret <- m
  atomically $ do
    t' <- readTVar turn
    writeTVar turn (t'+1)
  return ret


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

  --hPutStrLn stderr $ "hajimea" ++ (show $ utcToLocalTime tz ut)
  replicateM_ 3 $ hGetLine procOut >>= hPutStrLn stdout
  hFlush stdout
  --hPutStrLn stderr $ "owar"

play :: Int -> Handle -> Handle -> IO ()
play phase procIn procOut = do
  when (phase==1) $ getHands procIn
  forever $ do
    timeout (60*10^(6::Int)) $ putHands procOut
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
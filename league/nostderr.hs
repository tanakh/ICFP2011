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


lastTime :: TVar UTCTime
lastTime = unsafePerformIO $ do
             t <- getCurrentTime
             newTVarIO t
{-# NOINLINE lastTime #-}
isTimeout :: TVar Bool
isTimeout = unsafePerformIO $ newTVarIO False
{-# NOINLINE isTimeout #-}


killer :: ProcessID -> NominalDiffTime -> IO ()
killer pid dt = 
  forever $ do
    (t,b)  <- atomically $ do
                t <- readTVar lastTime
                b <- readTVar isTimeout
                return (t,b)
    t' <- getCurrentTime
    if (b && (t' `diffUTCTime` t) > dt) 
    then do 
      _ <- system $ "kill " ++ show pid
      return ()
    else return ()
    usleep 10000

trash :: Handle -> IO ()
trash hdl = forever $ hGetLine hdl >> return ()

getHands :: Handle -> IO ()
getHands procIn = do
  replicateM_ 3 $ hGetLine stdin >>= hPutStrLn procIn
  hFlush procIn
             

putHands :: Handle -> IO ()
putHands procOut = do
  t <- getCurrentTime
  atomically $ do
    writeTVar lastTime t
    writeTVar isTimeout True
  replicateM_ 3 $ hGetLine procOut >>= hPutStrLn stdout
  atomically $ do
    writeTVar isTimeout False
  hFlush stdout


play :: Int -> Handle -> Handle -> IO ()
play phase procIn procOut = do
  when (phase==1) $ getHands procIn
  forever $ do
    putHands procOut
    getHands procIn

main :: IO ()
main = do
  args <- getArgs
  let phase = (read $ last args)
  (Just procIn, Just procOut, Just procErr, hdl) <- createProcess 
    (shell $ unwords args) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  _ <- forkIO $ trash procErr
  pid <- getProcessID
  forkIO $ killer pid 60.0
  play phase procIn procOut
  _ <- waitForProcess hdl
  return ()
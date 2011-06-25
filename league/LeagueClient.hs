#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import Control.Monad
import Data.Maybe
import League 
import Network.MessagePackRpc.Client
import System.Environment
import System.Exit
import System.IO
import System.Process

suggestMatch :: RpcMethod (IO (String, String))
suggestMatch = method "suggestMatch"

reportMatch :: RpcMethod ((String, String) -> String -> IO ())
reportMatch = method "reportMatch"

seal :: String -> String
seal cmd = "'./nostderr " ++  cmd ++ "'"
--seal cmd = cmd

main :: IO ()
main = do
  conn <- connect "192.168.0.1" port
--  env <- getEnvironment
--  let debianicEnv = [("python","/usr/bin/python2")] ++ filter ((/="python") . fst) env
  forever $ do
    (cmd0, cmd1) <- suggestMatch conn 
    when (length cmd0 <= 0) exitSuccess
    let vs :: String -> String -> IO ()
        vs cmd0' cmd1' = do
              let cmd = "../bin/ltg -silent true match " ++ seal cmd0' ++ " " ++ seal cmd1'
              hPutStrLn stderr cmd
              (_, _, Just herr,hdl) <- createProcess (shell cmd) {std_err = CreatePipe {-, env = Just debianicEnv-} }
              ret <- hGetContents herr
              let result = last $ [""] ++ (filter ((=="!!").(take 2)) $ lines ret)
              _ <- length result `seq` waitForProcess hdl
              reportMatch conn (cmd0', cmd1') result
    cmd0 `vs` cmd1
    cmd1 `vs` cmd0






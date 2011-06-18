#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import Control.Monad
import League 
import Network.MessagePackRpc.Client
import System.IO
import System.Process

suggestMatch :: RpcMethod (IO (String, String))
suggestMatch = method "suggestMatch"

reportMatch :: RpcMethod ((String, String) -> String -> IO ())
reportMatch = method "reportMatch"

main :: IO ()
main = do
  conn <- connect "192.168.0.1" port
  forever $ do
    match@(cmd0, cmd1) <- suggestMatch conn 
    let cmd = "../bin/ltg -silent true match " ++  cmd0 ++ " " ++ cmd1
    hPutStrLn stderr cmd
    (_, _, Just herr,_) <- createProcess (shell cmd) {std_err = CreatePipe}
    ret <- hGetContents herr
    let result = last $ filter ((=="!!").(take 2)) $ lines ret
    reportMatch conn match result



#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import Control.Monad
import League 
import Network.MessagePackRpc.Client


suggestMatch :: RpcMethod (IO (String, String))
suggestMatch = method "suggestMatch"

main :: IO ()
main = do
  conn <- connect "192.168.0.1" port
  replicateM_ 1 $ do
    (cmd0, cmd1) <- suggestMatch conn 
    print (cmd0, cmd1)

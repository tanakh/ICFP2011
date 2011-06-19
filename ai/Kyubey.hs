#!/usr/bin/env runhaskell
import LTG

import qualified Control.Exception.Control as E
import Control.Monad


ignExc :: LTG a -> LTG ()
ignExc m = do
  mb <- E.try m
  case mb of
    Left (LTGError _) -> return ()
    Right _ -> return ()

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  forever $ do
    ignExc qbMain
    _ <- revive 0
    return ()



qbMain :: LTG ()
qbMain = do
  deadEvens  <- filterM (isDead  True) [0,2..128]
  aliveEvens <- filterM (isAlive True) [0,2..128]
  if null deadEvens
  then do
    num 0 2
    Inc $> 0
    num 0 4
    Inc $> 0
  else do
    _ <- revive (head deadEvens)
    return ()

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
{-
scan :: Int -> LTG (Int, Bool, Value)
scan i = do
  a    <- isAlive  True i
  val  <- getValue True i
  vit  <- getVital True i
-}

qbMain :: LTG ()
qbMain = do
  deadEvens  <- filterM (isDead  True) [0,2..128]
  aliveEvens <- filterM (isAlive True) [0,2..128]
  if nul 
  return ()

#!/usr/bin/env runhaskell
import LTG

import Control.Monad

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  num 1 255
  Get $> 0
  forever $ do
    b <- isAlive True 255
    if b then nop
    else do
      Revive $> 0
      Get $> 0
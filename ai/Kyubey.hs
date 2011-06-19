#!/usr/bin/env runhaskell
import LTG

import Control.Monad

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  forever $ do
    _ <- filterM (isAlive True) [0..255]
    as <- filterM (isAlive True) [0..255]
    let evens = map (2*)[0..]
    nop
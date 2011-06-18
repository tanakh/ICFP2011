#!/usr/bin/env runhaskell
import LTG

import Control.Monad

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  forever $ do
    I $> 0

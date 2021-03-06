{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
import Control.Applicative
import qualified Control.Exception.Control as E
import Control.Monad
import Control.Monad.State

import Data.Maybe

import LTG 

sittingDuck = do
  I $> 0
  sittingDuck

kyoukoMain :: LTG()
kyoukoMain = do
  -- S (K (S (S Attack (K 255)) (K 8192))) Get
  clear 1
  1 $< S
  1 $< Attack
  1 $< I
  S $> 1
  num 0 512
  K $> 0
  apply0 1
  0 $< Put

  Dbl $> 0
  Dbl $> 0
  Dbl $> 0
  Dbl $> 0
  K $> 0  

  clear 3
  3 $< S
  3 $< Help
  3 $< I
  S $> 3
  apply0 3

  clear 2
  2 $< Get
  clear 0
  0 $< Zero
  lazyApply 2 0
  S $> 2
  2 $< Succ
  copyTo 0 2

  S $> 3
  apply0 3

  copyTo 0 3
  S $> 1
  apply0 1

  copyTo 0 1

  1 $< Zero

  copyTo 1 0
  1 $< Zero

  copyTo 1 0
  1 $< Zero


--  num 0 2
--  1 $< Zero


  sittingDuck

main :: IO ()
main = runLTG $ do
  kyoukoMain

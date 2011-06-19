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
  1 $< Attack
  S $> 1
  num 0 255
  copyTo 2 0
  K $> 0
  apply0 1
  S $> 1
  0 $< Put
  -- num 0 256
  Dbl $> 0
  Dbl $> 0
  Dbl $> 0
  Dbl $> 0
  Dbl $> 0
  K $> 0
  apply0 1

  S $> 1

  copyTo 0 2
  Zombie $> 0
  apply0 1

  K $> 1
  S $> 1
  1 $< Get

  num 0 1
  num 2 1
  Get $> 2
  2 $< Zero
  num 0 2
  1 $< Zero


  sittingDuck

main :: IO ()
main = runLTG $ do
  kyoukoMain

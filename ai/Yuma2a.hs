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
  let f1 = 1
  let f3 = 2
  -- S (K (S (S Attack (K 255)) (K 8192))) Get

  -- v[f1] <- S Attack (K 255)
  clear f1
  f1 $< S
  f1 $< Attack
  num 0 255
  K $> 0
  apply0 f1
  0 $< Put

  -- v[f1] <- S (S Attack (K 255)) (K 255*2)
  S   $> f1
  Dbl $> 0
  K   $> 0
  apply0 f1
  0 $< Put

  -- v[f3] <- S (S Help I) (K 255*32)
  Dbl $> 0
  Dbl $> 0
  Dbl $> 0
  Dbl $> 0
  K $> 0
  clear f3
  f3 $< S
  f3 $< Help
  f3 $< I
  S  $> f3
  apply0 f3

  -- v[f3] <- (S v[f3] (S Get I))
  clear 0
  0 $< S
  0 $< Get
  0 $< I
  S $> f3
  apply0 f3

  -- S v[f1] v[f3]
  S $> f1
  copyTo 0 f3
  apply0 f1

  copyTo 0 f1

  forever $ do
    num 2 f1
    Get $> 2
    2 $< Zero

  sittingDuck

main :: IO ()
main = runLTG $ do
  kyoukoMain

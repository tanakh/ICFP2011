#!/usr/bin/env runhaskell
import LTG

import Control.Monad

summonMami :: Int -> Int -> Int -> LTG()
summonMami slot work n = do
  copyTo work slot
  num 0 (max 0 (255 - (n + 124)))
  apply0 work

prepareMagicalBullet :: Int -> Int -> LTG ()
prepareMagicalBullet slot work = do
  -- 
  -- S (\X -> dec X) (\X -> (lazy (get slot)) (succ X)) X
  -- compiles to
  -- S dec (S (S (K get) (K slot)) succ))
  clear work
  clear slot
  
  work $< Get
  
  num slot slot

  lazyApply work slot -- (S (K get) (K slot))

  S $> work
  work $< Succ -- S (S (K get) (K slot)) succ

  clear slot
  slot $< S
  slot $< Dec
  copyTo0 work
  apply0 slot -- S Dec (S (S ...))

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  prepareMagicalBullet 1 2
  forever $ do
    forM_ [0,90,180] $ \x -> do
      summonMami 1 2 x


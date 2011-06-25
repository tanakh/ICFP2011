#!/usr/bin/env runhaskell
import LTG

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import System.IO.Unsafe


i1, i2, i3, i255 :: Int
(i1: i2: i3: _) = [1..]
i255 = 255


openingSwitch :: IORef Bool
openingSwitch = unsafePerformIO $ newIORef True
{-# NOINLINE openingSwitch #-}

isOpening :: LTG Bool
isOpening = liftIO $ readIORef openingSwitch

main :: IO ()
main = runLTGs
       [(isOpening      , openingMove),
        (isAlive False 0, forever $ i1 $<< Get) ,
        (return True    , hima)
       ]


infix 1 $<<
($<<) :: Int -> Card -> LTG()
i $<< card = do
  ensureAlive i
  i $< card
  
infix 1 $>>
($>>) :: Card -> Int -> LTG()
card $>> i = do
  ensureAlive i
  card $> i
  
clearR :: Int -> LTG()
clearR ix = do
  f <- getField True ix
  when (f /= VFun "I") $ do
    Put $>> ix

applyR0 :: Int -> LTG ()
applyR0 i = do
  K $>> i
  S $>> i
  i $<< Get
  ensureAlive 0
  i $<< Zero

numCost :: Int -> Int
numCost 0 = 1
numCost i = 1 + (i `mod` 2) + (numCost $ i `div` 2)

hima :: LTG ()
hima = forever $ do
         t <- getTurnCnt
         b <- getVital False 0
         lprint $ "hima t: " ++ show t ++ " " ++ show b
         nop

{- Ikki ni Kimesasete
   Morauwayo ! -}
openingMove :: LTG ()
openingMove = do
  i2     $<< Zero
  i3     $<< Zero
  Succ   $>> i3
  Attack $>> i2
  Attack $>> i3
  -- 5

  0    $<< Zero
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  -- v[0] = 255; 16

  applyR0 i2
  applyR0 i3
  --applyR0 5
  -- 8 

  -- S (S Dec) (K 255)
  i1   $<< S
  i1   $<< Dec
  S    $>> i1
  K    $>> 0
  applyR0 i1
  0    $<< Put

  Dbl  $>> 0
  Dbl  $>> 0
  Dbl  $>> 0
  Dbl  $>> 0
  Dbl  $>> 0
  -- 5

  applyR0 i2
  applyR0 i3
  --applyR0 5
  -- 8 + 4
  copyTo i255 i1

  liftIO $ writeIORef openingSwitch False

  forever nop

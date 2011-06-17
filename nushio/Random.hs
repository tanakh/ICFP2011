{-# OPTIONS -Wall #-}

import Data.Vector ((!))
import qualified Data.Vector as V
import LTG
import Prelude hiding (putStrLn)
import System.Environment
import System.Random




main :: IO ()  
main = do
  (slotRange:seed:_) <- fmap (map read) getArgs
  setStdGen $ mkStdGen seed
  play slotRange

play :: Int -> IO ()
play slotRange = do
  ci <- randomRIO (0, V.length cards-1)
  s  <- randomRIO (0, slotRange-1)
  lr <- randomRIO (0, 1::Int)
  let c = cards ! ci
  if lr == 0 
  then (s $< c)
  else (c $> s)
  play slotRange


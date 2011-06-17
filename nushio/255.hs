{-# OPTIONS -Wall #-}
import Control.Monad
import LTG

main :: IO ()  
main = do
  1 $< Zero
  replicateM_ 7 $ do
         Succ $> 1
         Dbl $> 1
  Succ $> 1
  attack

attack :: IO ()
attack = do
  2 $< Zero
  Succ $> 2
  Get $> 2
  Dec $> 2
  attack

    


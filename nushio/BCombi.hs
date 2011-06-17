{-# OPTIONS -Wall #-}
import System.Posix.Unistd
import LTG

main :: IO ()  
main = do
  I $> 0
  0 $< K
  0 $< S
  S $> 0
  0 $< K
  0 $< Get
  0 $< Inc
  return ()

    


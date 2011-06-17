{-# OPTIONS -Wall #-}
import LTG

main :: IO ()  
main = do
  I $> 0
  0 $< K
  0 $< S
  S $> 0
  0 $< K
  0 $< Get
  0 $< Succ
  return ()

    


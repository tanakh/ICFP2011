{-# OPTIONS -Wall #-}
import LTG

-- Example: attack 3 4 10
main :: IO()
main = do
  -- v[1] <- (Attack 3)
  1 $< Zero
  Succ $> 1
  Dbl  $> 1
  Succ $> 1
  Attack $> 1
  -- v[0] <- 4
  0 $< Zero
  Succ $> 0
  Dbl $> 0
  Dbl $> 0
  -- v[1] <- apply v[1] v[0]
  K $> 1
  S $> 1
  1 $< Get
  1 $< Zero
  -- v[0] <- 10
  Succ $> 0
  Dbl $> 0
  -- v[1] <- apply v[1] v[0]
  K $> 1
  S $> 1
  1 $< Get
  1 $< Zero
  


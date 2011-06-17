{-# OPTIONS -Wall #-}
import LTG

-- Love me do
apply0 :: Int -> IO ()
apply0 field = do
  K $> field
  S $> field
  field $< Get
  field $< Zero

clear :: Int -> IO()
clear field = do
  Zero $> field

num :: Int -> Int -> IO ()
num field n = do
  clear field
  field $< Zero
  num_iter n
  where
    num_iter 0 = do
      return ()
    num_iter 1 = do
      Succ $> field
    num_iter q = do
      num_iter (q `div` 2)
      Dbl $> field
      if q `mod` 2 == 0 
        then return ()
        else (Succ $> field) >> (return ())

-- Example: attack 3 4 10
attack from to value = do
  -- v[1] <- (Attack from)
  num 1 from
  Attack $> 1
  -- v[0] <- to
  num 0 to
  -- v[1] <- apply v[1] v[0]
  apply0 1
  -- v[0] <- value
  num 0 value
  -- v[1] <- apply v[1] v[0]
  apply0 1

copytozero :: Int -> IO ()
copytozero field = do
  num 0 field
  Get $> 0

-- inject zombie that attack
inject_sayasaya :: Int -> IO ()
inject_sayasaya dmg = do
  -- v[7] <- S (K (attack 0 0))
  7 $< Attack
  7 $< Zero
  7 $< Zero
  K $> 7
  S $> 7
  -- v[0] <- K 256
  num 0 256
  K $> 0
  -- Sayasaya ready
  apply0 7
  -- Inject!
  8 $< Zero
  Zombie $> 8
  copytozero 7
  apply0 8

main :: IO()
main = do
  attack 3 0 8192
  attack 4 0 8192
  inject_sayasaya 256




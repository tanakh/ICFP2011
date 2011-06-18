module LTG.SoulGems (
                apply0,
                clear,
                num,
                attack,
                copyTo0,
                lazyApply,
                compose,
                lazyAdd,
                composeNtimes0,
                futureApply,
                lazyGet
) where

import LTG.Base
import LTG.Monad

-- Love me do
-- v[field] <- v[field] v[0]
-- S (K v[field]) Get Zero = v[field] (Get Zero)
-- Construct cost: 4
-- Execution cost: -
apply0 :: Int -> LTG ()
apply0 field = do
  K $> field
  S $> field
  field $< Get
  field $< Zero


-- v[field] <- I
clear :: Int -> LTG()
clear field = do
  Zero $> field


-- v[field] <- n
num :: Int -> Int -> LTG ()
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


-- execute "attack from to value" using v[0] and v[1]
-- Example: attack 3 4 10
attack :: Int -> Int -> Int -> LTG ()
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

-- v[0] <- v[field]
-- Construct cost: 2
-- Execution cost: -
copyTo0 :: Int -> LTG ()
copyTo0 field = do
  if field /= 0
    then do
      num 0 field
      Get $> 0
    else
      return ()

-- v[f1] <- (\x -> v[f1] v[f2])
-- (S (K v[f1]) (K v[f2])) = (\x -> v[f1] v[f2])
-- Construct cost: 3+2+4=9
-- Execution cost: +3
lazyApply :: Int -> Int -> LTG ()
lazyApply f1 f2 = do
  K    $> f1
  S    $> f1
  copyTo0 f2
  K    $> 0
  apply0 f1

-- v[f1] <- \x -> (v[f1] (v[f2] x))
-- S (K v[f1]) v[f2]
compose :: Int -> Int -> LTG ()
compose f1 f2 = do
  K    $> f1
  S    $> f1
  copyTo0 f2
  apply0  f1

-- v[field] <- \x -> v[field] (\x + n)
lazyAdd :: Int -> Int -> LTG ()
lazyAdd field n = do
  num_iter n
  where
    num_iter 0 = do
      return ()
    num_iter 1 = do
      clear 0
      0     $< Succ
      compose field 0
    num_iter q = do
      if q `mod` 2 == 0 
        then
          return ()
        else do
           clear 0
           0     $< Succ
           compose field 0
      clear 0
      0     $< Dbl
      compose field 0
      num_iter (q `div` 2)

-- v[f1] <- composion v[f1] v[0] ...(n times)... v[0]
composeNtimes0 :: Int -> Int -> LTG ()
composeNtimes0 f1 n = do
  if n <= 0
    then
      return ()
    else do
      K    $> f1
      S    $> f1
      apply0  f1
      composeNtimes0 f1 (n-1)

-- apply0:       v[field] <- v[field] (Get Zero)
-- future_apply: v[f1]    <- \x -> (Get (i1+x)) (Get (i2+x))
-- S (\x -> get (succ ... (succ \x))) (\x -> get (succ ... (succ \x))) zero
-- S (compisition get succ ... succ) (compisition get succ ... succ) zero
futureApply :: Int -> Int -> Int -> Int -> LTG ()
futureApply f1 f2 i1 i2 = do

  clear f1
  f1 $< Get
  lazyAdd f1 i1

  clear f2
  f2 $< Get
  lazyAdd f2 i2

  S $> f1

  copyTo0 f2
  apply0 f1

-- v[f1] <- (\x -> get target)
lazyGet :: Int -> Int -> Int -> LTG ()
lazyGet target f1 f2 = do -- (S (K Get) (K target)) = (\x -> get target) to f1
  clear f1
  f1   $< Get
  num f2 target
  lazyApply f1 f2

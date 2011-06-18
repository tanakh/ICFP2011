{-# OPTIONS -Wall #-}
import LTG hiding(($<), ($>))
import System.Environment
import System.IO
import System.Exit

skip :: IO ()
skip = do
  iseof <- hIsEOF stdin 
  if iseof 
    then exitSuccess 
    else do
    _ <- getLine
    _ <- getLine
    _ <- getLine
    return ()

($<) x y = do
  right x y
  skip
  
($>) x y = do
  left x y
  skip

-- Love me do
-- v[field] <- v[field] v[0]
-- S (K v[field]) Get Zero = v[field] (Get Zero)
-- Construct cost: 4
-- Execution cost: -
apply0 :: Int -> IO ()
apply0 field = do
  K $> field
  S $> field
  field $< Get
  field $< Zero

-- v[field] <- I
clear :: Int -> IO()
clear field = do
  Zero $> field

-- v[field] <- n
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

-- execute "attack from to value" using v[0] and v[1]
-- Example: attack 3 4 10
attack :: Int -> Int -> Int -> IO ()
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
copytozero :: Int -> IO ()
copytozero field = do
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
lazy_apply :: Int -> Int -> IO ()
lazy_apply f1 f2 = do
  K    $> f1
  S    $> f1
  copytozero f2
  K    $> 0
  apply0 f1

-- v[f1] <- \x -> (v[f1] (v[f2] x))
-- S (K v[f1]) v[f2]
composition :: Int -> Int -> IO ()
composition f1 f2 = do
  K    $> f1
  S    $> f1
  copytozero f2
  apply0  f1

-- v[field] <- \x -> v[field] (\x + n)
lazy_add :: Int -> Int -> IO ()
lazy_add field n = do
  num_iter n
  where
    num_iter 0 = do
      return ()
    num_iter 1 = do
      clear 0
      0     $< Succ
      composition field 0
    num_iter q = do
      if q `mod` 2 == 0 
        then
          return ()
        else do
           clear 0
           0     $< Succ
           composition field 0
      clear 0
      0     $< Dbl
      composition field 0
      num_iter (q `div` 2)

-- v[f1] <- composion v[f1] v[0] ...(n times)... v[0]
composition0_ntimes :: Int -> Int -> IO ()
composition0_ntimes f1 n = do
  if n <= 0
    then
      return ()
    else do
      K    $> f1
      S    $> f1
      apply0  f1
      composition0_ntimes f1 (n-1)

-- apply0:       v[field] <- v[field] (Get Zero)
-- future_apply: v[f1]    <- \x -> (Get (i1+x)) (Get (i2+x))
-- S (\x -> get (succ ... (succ \x))) (\x -> get (succ ... (succ \x))) zero
-- S (compisition get succ ... succ) (compisition get succ ... succ) zero
future_apply :: Int -> Int -> Int -> Int -> IO ()
future_apply f1 f2 i1 i2 = do

  clear f1
  f1 $< Get
  lazy_add f1 i1

  clear f2
  f2 $< Get
  lazy_add f2 i2

  S $> f1

  copytozero f2
  apply0 f1

-- v[f1] <- (\x -> get target)
lazy_get :: Int -> Int -> Int -> IO ()
lazy_get target f1 f2 = do -- (S (K Get) (K target)) = (\x -> get target) to f1
  clear f1
  f1   $< Get
  num f2 target
  lazy_apply f1 f2

-- inject zombie that attack
inject_sayasaya :: Int -> Int -> Int -> Int -> Int -> IO ()
inject_sayasaya f1 f2 fdmg fgain dmg = do
  -- v[f1] <- S (K (attack fdmg fgain))
  num f1 fdmg
  Attack $> f1
  num 0 (255 - fgain)
  apply0 f1
  K $> f1
  S $> f1
  -- v[0] <- K dmg
  num 0 dmg
  K $> 0
  -- Sayasaya ready
  apply0 f1
  -- Inject!
  f2 $< Zero
  Zombie $> f2
  copytozero f1
  apply0 f2

-- I want to revive Sayaka!
revive_sayasaya :: Int -> Int -> IO ()
revive_sayasaya f1 f2 = do
  clear 145
  145    $< Zero
  Revive $> 145

  clear 145
  145    $< Zero
  Succ   $> 145
  Revive $> 145

  clear 145
  145    $< Zero
  Succ   $> 145
  Succ   $> 145
  Revive $> 145

  lazy_get f2 f1 f2
  S    $> f1
  f1   $< Succ
  clear f2
  f2   $< Revive -- the function you wish to execute!
  S    $> f2
  copytozero f1
  apply0 f2
  copytozero f2
  0   $< Zero
  num 0 128
  num f1 f2
  Get $> f1
  apply0 f1

-- (S f) ((S (K (get zero))) succ)
-- (S f) ((S (S (K get) (K zero))) succ) / 8
-- S put S f (S put (S (S (S put K get) (S put K zero))) succ) / 8
-- 
-- \x (f x) ((\y -> get zero) (succ x))
-- \x -> (get zero) (succ x)
-- \x -> S (get zero) (succ x)
-- \x -> S (get zero) (succ) x
-- \x -> (get (first zero \x)) (succ \x)
-- S (get zero) succ
-- \x -> (f x; ((\y -> get zero) x) (succ x))
-- F x y = F (succ x) (f x)
-- F x = F (succ ((f x) x))
-- (\x -> get zero) (succ x) (f x)

sittingDuck :: IO()
sittingDuck = do
  I $> 0
  sittingDuck

attackloop :: Int -> Int -> Int -> IO()
attackloop v k s = do
  attack v k 8192
  attack (v+1) k 8192
  inject_sayasaya 3 4 s s 10000
  if v > 240 
    then do
      revive_sayasaya 1 2
      attackloop 5 (k+1) 0
    else attackloop (v+2) (k+1) (s+1)

main :: IO()
main = do
  [arg] <- getArgs
  let b = (read arg :: Int) -- 0: Sente, 1: Gote
  if b == 1 then skip else return ()
  3      $< Zero
  Succ   $> 3
  4      $< Zero
  Succ   $> 4
  Succ   $> 4
  Attack $> 3
  Attack $> 4
  -- 7

  0    $< Zero
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  -- v[0] = 255; 16

  apply0 3
  apply0 4
  -- 8

  Dbl  $> 0
  Dbl  $> 0
  Dbl  $> 0
  Dbl  $> 0
  Dbl  $> 0
  -- 5

  apply0 3
  apply0 4
  -- 8

{-
  future_apply 3 1 1 2
  clear 1
  1 $< Inc
  clear 2
  2 $< Zero
  Succ $> 2
  Succ $> 2
  Succ $> 2
  Succ $> 2
  Succ $> 2
  Succ $> 2
  Succ $> 2
  Succ $> 2
  Succ $> 2
  Succ $> 2
  3 $< Zero
-}
  attackloop 5 0 0
  sittingDuck




{-# OPTIONS -Wall #-}
import LTG hiding(($<), ($>))
import System.Environment
import System.IO
import System.Exit
import System.Random

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

attackCode :: Int -> IO ()
attackCode f = do
  clear f
  r <- randomRIO (0, 2 :: Int)
  target <- randomRIO (0, 255 :: Int)
  drainfrom <- randomRIO (0, 255 :: Int)
  value <- randomRIO (0,13 :: Int)
  let q = 2 ^ value
  let arity3 card = do
        num f target
        card $> f
        num 0 drainfrom
        apply0 f
        K $> f
        S $> f
        num 0 q
        K $> 0
        apply0 f
  let arity1 card = do
        num 0 target
        K $> 0
        f $< card
        K $> f
        S $> f
        apply0 f
  case r of
    0 ->
      arity3 Help
    1 -> 
      arity3 Attack
    2 ->
      arity1 Dec
    3 ->
      arity1 Inc

-- inject zombie that attack
inject_sayasaya :: Int -> Int -> IO ()
inject_sayasaya f1 f2 = do
  attackCode f1
  -- Inject!
  f2 $< Zero
  Zombie $> f2
  copytozero f1
  apply0 f2

get_closure :: Int -> Int -> Int -> IO ()
get_closure target f1 f2 = do -- (S (K Get) (K target)) = (\x -> get target) to f1
  clear f1
  f1   $< Get
  K    $> f1
  S    $> f1
  num f2 target
  K    $> f2
  copytozero f2
  apply0 f1

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

  get_closure f2 f1 f2
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

{-
heal_sayasaya :: Int -> Int -> IO ()
heal_sayasaya f1 f2 = do
  clear f1
  f1   $< Get
  K    $> f1
  S    $> f1
  clear f2
  f2   $< Zero
  K    $> f2
  copytozero f2
  apply0 f1
  S    $> f1

  clear f2
  f2   $< S
  f2   $< Inc
  f2   $< Succ
  copytozero f2
  apply0 f1
  copytozero f1
  f1   $< Zero
-- (S f) ((S (S (K get) (K zero))) succ) / 8
-- S (\x -> get zero) (S f succ)
-- (S (K get) (K zero)) (S f succ) / 6
-- (\x -> get zero)
-- (S f hoge) = \x (hoge x)
-- S (\x -> get zero) (\x -> succ ((f x) x)) x
-- => (get 0) (f x; succ x)
-}




sittingDuck :: IO()
sittingDuck = do
  I $> 0
  sittingDuck

revive_sayasayaloop :: IO()
revive_sayasayaloop = do
  revive_sayasaya 1 2
  revive_sayasayaloop
  
attackloop :: Int -> Int -> Int -> IO()
attackloop v k s = do
  attack v k 8192
  attack (v+1) k 8192
  inject_sayasaya 3 4
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
  attackloop 5 0 0
  sittingDuck




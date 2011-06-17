{-# OPTIONS -Wall #-}
import Data.IORef
import Data.Vector ((!))
import qualified Data.Vector as V
import LTG hiding(($<), ($>))
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Exit
import System.Random


randRatio :: IORef Double
randRatio = unsafePerformIO (newIORef 0)

slotRange :: IORef Int
slotRange = unsafePerformIO (newIORef 0)

unstable :: IO ()
unstable = return ()
{-do
  rr <- readIORef randRatio
  drand <- randomRIO (0,1)
  if drand <= rr
  then return ()
  else do
    sr <- readIORef slotRange
    ci <- randomRIO (0, V.length cards-1)
    s  <- randomRIO (0, sr-1)
    lr <- randomRIO (0, 1::Int)
    let c = cards ! ci
    if lr == 0 
    then (s $< c)
    else (c $> s)
    skip
  -}

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

infix 3 $<
infix 3 $>

($<) :: Int -> Card -> IO ()
($<) x y = do
  right x y
  skip
  unstable
  
($>) :: Card -> Int -> IO ()  
($>) x y = do
  left x y
  skip
  unstable

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

copytozero :: Int -> IO ()
copytozero field = do
  num 0 field
  Get $> 0

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
  inject_sayasaya 3 4 s s 10000
  if v > 240 
    then do
      revive_sayasaya 1 2
      attackloop 5 (k+1) 0
    else attackloop (v+2) (k+1) (s+1)

main :: IO()
main = do
  (rr: sr: seed: arg: _) <- getArgs
  writeIORef randRatio (read rr)
  writeIORef slotRange (read sr)
  setStdGen $ mkStdGen (read seed)
  let b = (read arg :: Int) -- 0: Sente, 1: Gote
  if b == 1 then skip else return ()
  attackloop 5 0 0
  sittingDuck

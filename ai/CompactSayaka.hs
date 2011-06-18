{-# OPTIONS -Wall #-}
import LTG 
import LTG.SoulGems




-- inject zombie that attack
inject_sayasaya :: Int -> Int -> Int -> Int -> Int -> LTG ()
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
  copyTo0 f1
  apply0 f2

-- I want to revive Sayaka!
revive_sayasaya :: Int -> Int -> LTG ()
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

  lazyGet f2 f1 f2
  S    $> f1
  f1   $< Succ
  clear f2
  f2   $< Revive -- the function you wish to execute!
  S    $> f2
  copyTo0 f1
  apply0 f2
  copyTo0 f2
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

sittingDuck :: LTG()
sittingDuck = do
  I $> 0
  sittingDuck

attackLoop :: Int -> Int -> Int -> LTG()
attackLoop v k s = do
  attack v k 8192
  attack (v+1) k 8192
  inject_sayasaya 3 4 s s 10000
  if v > 240 
    then do
      revive_sayasaya 1 2
      attackLoop 5 (k+1) 0
    else attackLoop (v+2) (k+1) (s+1)

main :: IO ()
main = runLTG $ do
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

  attackLoop 5 0 0
  sittingDuck




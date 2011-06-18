{-# OPTIONS -Wall #-}
import LTG 
import LTG.SoulGems

zombieLoop f2 = do
  num 7 0
  copyTo f2 0
  f2 $< I

  num 7 43
  copyTo f2 0
  f2 $< I

  num 7 86
  copyTo f2 0
  f2 $< I

  num 7 129
  copyTo f2 0
  f2 $< I

  num 7 172
  copyTo f2 0
  f2 $< I

  num 7 215
  copyTo f2 0
  f2 $< I

  zombieLoop f2


kyokoAnAn :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
kyokoAnAn f1 f2 f3 f4 f5 f7 target = do
-- f1, f2, f3: temp
-- f4, f5
-- target: zombie target
  -- \x -> (copy f4) (succ x)
  -- next = v[f2] <- S (lazy_apply Copy f4) succ
  num f1 f4
  clear f2
  f2 $< Copy
  lazyApply f2 f1
  S $> f2
  f2 $< Succ

  -- f = v[f4] <- S (lazy_apply Copy f5) I
  num f1 f5
  clear f4
  f4 $< Copy
  lazyApply f4 f1
  S  $> f4
  f4 $< I

 -- v[f4] <- S f next
  S  $> f4
  copyTo 0 f2
  apply0 f4

  -- v[f1] <- S (lazyApply Copy f4) (lazyApply Copy f7)
  -- this is zombie!
  clear f1
  f1 $< Copy
  num f2 f4
  lazyApply f1 f2
  S $> f1

  clear f2
  f2 $< Copy
  num f3 f7
  lazyApply f2 f3
  copyTo 0 f2
  apply0 f1
--  lazyApply2 f1 f2 f3

  num f2 (255-target)
  Zombie $> f2

  lazyApply f2 f1
  copyTo 0 f2
  zombieLoop f2

sittingDuck :: LTG()
sittingDuck = do
  I $> 0
  sittingDuck

main :: IO ()
main = runLTG $ do
--  futureApply 1 2 18 3

  attack      1 0 8192
  attack      2 0 8192

-- v[5] <- S (S help I) (lazyApply Copy 6)

  clear 5
  5 $< S
  5 $< Help
  5 $< I
  S $> 5
  clear 6
  6 $< Copy
  num 7 6
  lazyApply 6 7
  copyTo 0 6
  apply0 5

  num 6 8192

  kyokoAnAn 1 2 3 4 5 7 255

--  attackFA    1 2 18 3 5 6 8192
--  attackLoopFA 1 2 18 5 0 0
--  sittingDuck

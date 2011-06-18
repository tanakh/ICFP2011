{-# OPTIONS -Wall #-}
import LTG 
import LTG.SoulGems

findFirstAlive :: Int -> LTG Int
findFirstAlive i = do
  if i >= 256
    then return (-1)
    else do
      v <- getVital False i
      if v > 0
        then return i
        else findFirstAlive (i+1)

getTotalDamage :: Int -> Int -> Int -> Int -> LTG Int
getTotalDamage pos0 d i total = do
  if i >= 256 || i >= pos0 + 58
    then return total
    else do
      v <- getVital False i
      if v < d
        then return total
        else if v <= d * 2
          then getTotalDamage pos0 d (i+1) (total+v)
          else getTotalDamage pos0 d (i+1) (total+d+d)

getOptimumDamage :: Int -> Int -> Int -> Int -> LTG Int
getOptimumDamage i d bestD bestTotal = do
  t <- getTotalDamage i d i 0
  if d >= 10000 || t == 0
    then return bestD
    else if t >= bestTotal
      then getOptimumDamage i (d+1) d     t
      else getOptimumDamage i (d+1) bestD bestTotal

{-
findDamage :: Int Int Int -> LTG Int
findDamage pos0 i lowestV bestV bestProd = do
  if i >= 256
    then return bestV
    else do
      v <- getVital False i
      let lowestV2   = if v >= lowestV then lowestV else v
      let bestProdV2 = lowestV2 * (i+1-pos0)
      if bestProd < lowestV * (i+1-pos0)
      
        then findDamage pos0 (i+1) lowestV ( then bestV
        else findDamage pos0 (i+1) v       bestV
-}

zombieLoopFA :: Int -> Int -> Int -> Int -> LTG ()
zombieLoopFA f2 f3 f6 f7 = do
  firstI <- findFirstAlive 0
  dmg    <- getOptimumDamage firstI 1 1 0
  num f6 dmg
  num f7 firstI

  copyTo f3 f2
  f3 $< I

{-
  num f7 58
  copyTo f3 f2
  f3 $< I
-}

  zombieLoopFA f2 f3 f6 f7


kyokoAnAnFA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
kyokoAnAnFA i1 i2 ffa f1 f2 f4 f6 f7 target = do

--  kyokoAnAnFA 4 8 16 6 7 5 6 7 255

-- f1, f2: temp
-- f4
-- target: zombie target
  -- \x -> (copy f4) (succ x)
  -- next = v[f2] <- S (lazy_apply Copy f4) succ
  num i2 f4
  clear i1
  i1 $< Copy
  lazyApplyFA i1 i2 ffa f2 i1 i2
  S $> f2
  f2 $< Succ

  -- f = v[f4] <- S (lazy_apply Copy f5) I
  -- S (S help I) (S (K copy) (K 6))

-- S (S(S help I)(S(K copy)(K 6))) (S (S(K copy)(K 4)) succ)
-- \x -> help x x (\x -> (copy 6)) x; (copy 4) (succ x)

  clear i1
  i1 $< Copy
  num i2 f6
  lazyApplyFA i1 i2 ffa f1 i1 i2
  clear i1
  i1 $< S
  i1 $< Help
  i1 $< I
  S $> i1
  applyFA i1 i2 ffa f4 i1 f1

 -- v[f4] <- S f next
  S  $> f4
  applyFA i1 i2 ffa f4 f4 f2

  -- v[f1] <- S (lazyApply Copy f4) (lazyApply Copy f7)
  -- this is zombie!
  clear i1
  i1 $< Copy
  num i2 f4
  lazyApplyFA i1 i2 ffa f1 i1 i2
  S $> f1

  clear i1
  i1 $< Copy
  num i2 f7
  lazyApplyFA i1 i2 ffa f2 i1 i2
  applyFA i1 i2 ffa f1 f1 f2

  num i1 (255-target)
  Zombie $> i1

  lazyApplyFA i1 i2 ffa f2 i1 f1
  copyTo i1 f2
  zombieLoopFA i1 i2 f6 f7


sittingDuck :: LTG()
sittingDuck = do
  I $> 0
  sittingDuck

futureApply2 :: Int -> Int -> Int -> Int -> LTG ()
futureApply2 i1 i2 ffa f2 = do
  ffa $< Get
  lazyAdd ffa i1
  num 1 ffa
  Get $> 1
  clear 0
  0     $< Succ
  compose 1 0
  copyTo 0 1
  S $> ffa
  apply0 ffa


main :: IO ()
main = runLTG $ do
  futureApply2 2 4 16 3
  attackFA     2 4 16 3 8 0 8192
  attackFA     2 4 16 3 16 0 8192
  kyokoAnAnFA  2 4 16 10 9 5 6 7 255

-- v[5] <- S (S help I) (lazyApply Copy 6)

--  attackFA    1 2 18 3 5 6 8192
--  attackLoopFA 1 2 18 5 0 0
--  sittingDuck

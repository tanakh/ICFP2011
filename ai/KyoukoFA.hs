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

getTotalDamage :: Int -> Int -> Int -> LTG Int
getTotalDamage d i total = do
  if i >= 256
    then return total
    else do
      v <- getVital False i
      if v < d
        then return total
        else if v <= d * 2
          then getTotalDamage d (i+1) (total+v)
          else getTotalDamage d (i+1) (total+d+d)

getOptimumDamage :: Int -> Int -> Int -> Int -> LTG Int
getOptimumDamage i d bestD bestTotal = do
  t <- getTotalDamage d i 0
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

  num f7 116
  copyTo f3 f2
  f3 $< I

  num f7 174
  copyTo f3 f2
  f3 $< I

  num f7 232
  copyTo f3 f2
  f3 $< I
-}

  zombieLoopFA f2 f3 f6 f7


kyokoAnAnFA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
kyokoAnAnFA i1 i2 ffa f1 f2 f3 f4 f6 f7 target = do
-- f1, f2, f3: temp
-- f4
-- target: zombie target
  -- \x -> (copy f4) (succ x)
  -- next = v[f2] <- S (lazy_apply Copy f4) succ
  num f1 f4
  clear f2
  f2 $< Copy
  lazyApplyFA i1 i2 ffa f2 f1
  S $> f2
  f2 $< Succ

  -- f = v[f4] <- S (lazy_apply Copy f5) I
  -- S (S help I) (S (K copy) (K 6))

-- S (S(S help I)(S(K copy)(K 6))) (S (S(K copy)(K 4)) succ)
-- \x -> help x x (\x -> (copy 6)) x; (copy 4) (succ x)

  clear f4
  f4 $< S
  f4 $< Help
  f4 $< I
  S $> f4
  clear f3
  f3 $< Copy
  num f1 f6
  lazyApplyFA i1 i2 ffa f3 f1
  applyFA i1 i2 ffa f4 f4 f3

 -- v[f4] <- S f next
  S  $> f4
  applyFA i1 i2 ffa f4 f4 f2

  -- v[f1] <- S (lazyApply Copy f4) (lazyApply Copy f7)
  -- this is zombie!
  clear f1
  f1 $< Copy
  num f2 f4
  lazyApplyFA i1 i2 ffa f1 f2
  S $> f1

  clear f2
  f2 $< Copy
  num f3 f7
  lazyApplyFA i1 i2 ffa f2 f3
  applyFA i1 i2 ffa f1 f1 f2
--  lazyApply2 f1 f2 f3

  num f2 (255-target)
  Zombie $> f2

  lazyApplyFA i1 i2 ffa f2 f1
  zombieLoopFA f2 f3 f6 f7


sittingDuck :: LTG()
sittingDuck = do
  I $> 0
  sittingDuck

main :: IO ()
main = runLTG $ do
  futureApply 1 2 4 3

  attack      1 0 8192
  attack      2 0 8192

-- v[5] <- S (S help I) (lazyApply Copy 6)

  kyokoAnAnFA 1 2 4 3 5 9 8 6 7 255

--  attackFA    1 2 18 3 5 6 8192
--  attackLoopFA 1 2 18 5 0 0
--  sittingDuck

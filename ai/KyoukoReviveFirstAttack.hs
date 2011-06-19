{-# OPTIONS -Wall #-}
import Control.Applicative
import qualified Control.Exception.Control as E
import Control.Monad
import Data.List
import Data.Maybe

import LTG 
import LTG.SoulGems

getFirstWorthEnemy :: Int -> LTG (Maybe Int)
getFirstWorthEnemy dmg = do
  alives <- filterM 
            (\ix -> do
                al <- isAlive False ix
                vt <- getVital False ix
                return (al && vt >= dmg))
            [0..255]
  if null alives then return Nothing
    else return $ Just $ head alives

getTotalDamage :: Int -> Int -> Int -> Int -> LTG Int
getTotalDamage pos0 d i total = do
  if i >= 256 || i >= pos0 + 58
    then return total
    else do
      v <- getVital False i
      if v < d
        then return total
        else if v <= d * 21 `div` 10
          then getTotalDamage pos0 d (i+1) (total+1)
          else getTotalDamage pos0 d (i+1) (total)
{-
          then getTotalDamage pos0 d (i+1) (total+v)
          else getTotalDamage pos0 d (i+1) (total+d+d)
-}
getOptimumDamage :: Int -> Int -> Int -> Int -> LTG Int
getOptimumDamage i d bestD bestTotal = do
  t <- getTotalDamage i d i 0
  if d >= 10000 || t == 0
    then return bestD
    else if t >= bestTotal
      then getOptimumDamage i (d+1) d     t
      else getOptimumDamage i (d+1) bestD bestTotal



zombieLoop :: Int -> Int -> LTG ()
zombieLoop f2 dmg = do
  elms <- getFirstWorthEnemy dmg
  case elms of
    Nothing -> return ()
    Just n -> do
      num 7 n
      copyTo f2 0
      f2 $< I
      zombieLoop f2 dmg

kyokoAnAn :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
kyokoAnAn f1 f2 f3 f4 f5 f7 target dmg = do
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
  num f1 6
  lazyApply f3 f1
  copyTo 0 f3
  apply0 f4

{-
  num f1 f5
  clear f4
  f4 $< Copy
  lazyApply f4 f1
  S  $> f4
  f4 $< I
-}

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
  zombieLoop f2 dmg

kyokoFirstAttack :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
kyokoFirstAttack f1 f2 f3 f4 f5 f7 target dmg = do
  -- \x -> (copy f4) (succ x)
  -- next = v[f2] <- S (lazy_apply Copy f4) succ
  num f1 f4
  clear f2
  f2 $< Copy
  lazyApply f2 f1
  S $> f2
  f2 $< Succ

  -- f = v[f4] <- S (lazy_apply Copy f5) I
  -- S (S help I) (S (K copy) (K 6))

-- S (S(S help I)(K 8192)) (S (S(K copy)(K 4)) succ)
-- \x -> help x x 8192 x; (copy 4) (succ x)

{-
  num f1 f5
  clear f4
  f4 $< Copy
  lazyApply f4 f1
  S  $> f4
  f4 $< I
-}

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

{-
  clear f2
  f2 $< Copy
  num f3 f7
  lazyApply f2 f3
  copyTo 0 f2
-}
  clear 0
  0 $< Zero
  K $> 0
  apply0 f1
--  lazyApply2 f1 f2 f3

  num f2 (255-target)
  Zombie $> f2

  clear f4
  f4 $< S
  f4 $< Help
  f4 $< I
  S $> f4

  dmg2 <- getOptimumDamage 0 1 1 0

  num 0 dmg2
  K $> 0
{-
  clear f3
  f3 $< Copy
  num f1 6
  lazyApply f3 f1
  copyTo 0 f3
-}

  apply0 f4


  copyTo 0 f1
  apply0 f2
{-
  lazyApply f2 f1
  copyTo 0 f2
  zombieLoop f2 dmg
-}

sittingDuck :: LTG()
sittingDuck = do
  I $> 0
  sittingDuck

-- get 3 * 2^n or 2^n smaller than x
getEasyInt :: Int -> Int
getEasyInt x = 
  max (head $ filter (\y -> y * 2 > x) twos) (head $ filter (\y -> y * 2 > x) threep)
  where
    twos = map (2^) [(0::Int)..]
    threep = map (\n -> 3*(2^n)) [(0::Int)..]

getMaxEnemy :: LTG Int
getMaxEnemy = do
  oppAlives <- filterM (isAlive False) [0..255]
  vitals <- mapM (getVital False) oppAlives
  return $ maximum vitals

kyoukoMain :: LTG()
kyoukoMain = do
  dmg <- getEasyInt <$> getMaxEnemy
  zombifySlotVital <- getVital False 255
  let zombifySlotV = getEasyInt zombifySlotVital
  alives <- filterM (\x -> do 
                        v <- getVital True x
                        return $ v > zombifySlotV)
            [1..255]
  -- TODO: raise error to increase vitality
  if length alives < 2 
    then return ()
    else do
    attack      (alives !! 0) 0 zombifySlotV
    attack      (alives !! 1) 0 zombifySlotV

    -- v[5] <- S (S help I) (lazyApply Copy 6)

{-
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
-}

    kyokoFirstAttack 1 2 3 4 5 7 255 8192

    num 6 dmg
    kyokoAnAn 1 2 3 4 5 7 255 dmg

--  attackFA    1 2 18 3 5 6 8192
--  attackLoopFA 1 2 18 5 0 0
--  sittingDuck
  

main :: IO ()
main = runLTG $ do
  forever $ do
    ds <- filterM (isDead True) [0..255]
    if null ds
      then do
      mb <- E.try kyoukoMain
      case mb of
        Left (LTGError e) -> do
          lprint e
          return ()
        Right _ -> do
          return ()
      return ()
      else do
      revive (head ds)
      return ()

--  futureApply 1 2 18 3


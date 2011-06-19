module LTG.SoulGems (
  apply0,
  clear,
  num,
  attack,
  attack2,
  copyTo,
  copyTo0,
  lazyApply,
  lazyApply2,
  compose,
  lazyAdd,
  composeNtimes0,
  futureApply,
  lazyGet,
  applyFA,
  attackFA,
  revive,
  lazyApplyFA,
  lazyApply2FA
) where

import Control.Monad
import LTG.Base
import LTG.Monad
import LTG.Simulator

import Data.Maybe

-- ################################################################
-- Functions that do NOT require v[0]
-- v[field] <- n
num :: Int -> Int -> LTG ()
num ix n = do
  f <- getField True ix
  case f of
    VInt cur | cur == n -> return () -- do Nothing
    VInt 0 -> 
      num_iter n
    _ -> do
      clear ix
      ix $< Zero
      num_iter n
  where
    num_iter 0 = do
      return ()
    num_iter 1 = do
      Succ $> ix
    num_iter q = do
      num_iter (q `div` 2)
      Dbl $> ix
      if q `mod` 2 == 0
        then return ()
        else (Succ $> ix) >> (return ())

-- v[field] <- I
clear :: Int -> LTG()
clear ix = do
  f <- getField True ix
  when (f /= VFun "I") $ do
    Put $> ix

-- v[f1] <- v[f2]
-- Construct cost: 2
-- Execution cost: -
copyTo :: Int -> Int -> LTG ()
copyTo f1 f2 = do
  ensureAlive f1
  ensureAlive f2
  if f1 /= f2
    then do
      num f1 f2
      Get $> f1
    else
      return ()

-- v[0] <- v[f2]
copyTo0 :: Int -> LTG ()
copyTo0 f2 = copyTo 0 f2

-- ################################################################
-- Apply0

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
  ensureAlive 0
  field $< Zero

-- ################################################################
-- Functions that require apply0

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

-- execute "attack2 from1/from2 to value" using v[0] and v[1] and f[2]
attack2 :: Int -> Int -> Int -> Int -> LTG ()
attack2 from1 from2 to value = do
  -- v[1] <- (Attack from)
  num 1 from1
  Attack $> 1
  num 2 from2
  Attack $> 2
  -- v[0] <- to
  num 0 to
  -- v[1] <- apply v[1] v[0]
  apply0 1
  apply0 2
  -- v[0] <- value
  num 0 value
  -- v[1] <- apply v[1] v[0]
  apply0 1
  apply0 2

-- v[f1] <- (\x -> v[f1] v[f2])
-- (S (K v[f1]) (K v[f2])) = (\x -> v[f1] v[f2])
-- Construct cost: 3+2+4=9
-- Execution cost: +3
lazyApply :: Int -> Int -> LTG ()
lazyApply f1 f2 = do
  K    $> f1
  S    $> f1
  copyTo 0 f2
  K    $> 0
  apply0 f1
  
-- v[f1] <- (\x -> v[f1] v[f2] v[f3])
-- (S (S (K v[f1]) (K v[f2])) (K v[f3]))
lazyApply2 :: Int -> Int -> Int -> LTG()
lazyApply2 f1 f2 f3 = do
  lazyApply f1 f2
  S    $> f1
  copyTo 0 f3
  K    $> 0
  apply0 f1
  
-- TODO: make lazyApplyList

-- v[f1] <- \x -> (v[f1] (v[f2] x))
-- S (K v[f1]) v[f2]
compose :: Int -> Int -> LTG ()
compose f1 f2 = do
  K    $> f1
  S    $> f1
  copyTo 0 f2
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
-- future_apply: v[ffa]   <- \x -> (Get (i1+x)) (Get (i2+x))
-- S (\x -> get (succ ... (succ \x))) (\x -> get (succ ... (succ \x))) zero
-- S (compisition get succ ... succ) (compisition get succ ... succ) zero
futureApply :: Int -> Int -> Int -> Int -> LTG ()
futureApply i1 i2 ffa f2 = do

  clear ffa
  ffa $< Get
  lazyAdd ffa i1

  clear f2
  f2 $< Get
  lazyAdd f2 i2
{-
  clear ffa
  ffa $< Get
  lazyAdd ffa i1

  clear f2
  f2 $< Get
  lazy_add f2 i2
-}
{-
  if i2 `mod` i1 == 0
    then do
      clear ffa
      ffa $< Get
      lazyAdd ffa i1
      num f2 ffa
      Get $> f2
      lazyAdd f2 (i2 `div` i1)
    else do -- i2 <= i1
      clear ffa
      ffa $< Get
      lazyAdd ffa i1
      clear f2
      f2 $< Get
      lazyAdd f2 i2
-}

  S $> ffa

  copyTo 0 f2
  apply0 ffa

-- v[f1] <- (\x -> get target)
lazyGet :: Int -> Int -> Int -> LTG ()
lazyGet target f1 f2 = do -- (S (K Get) (K target)) = (\x -> get target) to f1
  clear f1
  f1   $< Get
  num f2 target
  lazyApply f1 f2

-- ################################################################
-- Functions that require apply0, rewrited using futureApply i1 i2 at v[ffa]

-- v[fout] <- v[f1] v[f2]
-- not the order of argument - this is to make "applyFA f f 0" similar to "apply0 f"
-- fout != i1 && fout != i2
applyFA :: Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
applyFA i1 i2 ffa fout f1 f2 = do
  if i1 == f2 && i2 == f1
    then do
      return ()
    else if i1 == f2
      then do
        copyTo i2 f2
        copyTo i1 f1
      else do
        copyTo i1 f1
        copyTo i2 f2
  copyTo fout ffa
  fout $< Zero

-- execute "attack from to value" using v[0] and v[1]
-- Example: attack 3 4 10
-- f1: working field
attackFA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
attackFA i1 i2 ffa f1 from to value = do
  -- v[i1] <- (Attack from)
  num i1 from
  Attack $> i1
  -- v[i2] <- to
  num i2 to
  -- v[i1] <- apply v[i1] v[i2]
  applyFA i1 i2 ffa f1 i1 i2
  -- v[i2] <- value
  num i2 value
  -- v[i1] <- apply v[f1] v[i2]
  applyFA i1 i2 ffa f1 f1 i2

-- revive specified field
revive :: Int -> LTG Bool
revive ix = do
  a <- isAlive True ix
  if a then return True
    else do
    jx <- findIdentAlive
    case jx of
      Just jx' -> reviveImpl jx'
      Nothing -> do 
        -- This means almost all fields are filled with non-ident
        jx' <- findAlive True (const True)
        reviveImpl jx'
  where
    reviveImpl jx = do
      num jx ix
      Revive $> jx
      return False
    findIdentAlive = do
      aliveidents <- filterM 
                     (\ix -> do
                         al <- isAlive True ix
                         fn <- getField True ix
                         return (al && fn == VFun "I"))
                     [0..255]
      return $ listToMaybe aliveidents

-- v[f1] <- (\x -> v[f1] v[f2])
-- (S (K v[f1]) (K v[f2])) = (\x -> v[f1] v[f2])
-- Construct cost: 3+2+4=9
-- Execution cost: +3
lazyApplyFA :: Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
lazyApplyFA i1 i2 ffa fout f1 f2 = do
  K    $> f1
  S    $> f1
  K    $> f2
  applyFA i1 i2 ffa fout f1 f2

-- v[f1] <- (\x -> v[f1] v[f2] v[f3])
-- (S (S (K v[f1]) (K v[f2])) (K v[f3]))
lazyApply2FA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> LTG()
lazyApply2FA i1 i2 ffa fout f1 f2 f3 = do
  lazyApplyFA i1 i2 ffa fout f1 f2
  S    $> fout
  K    $> f3
  applyFA i1 i2 ffa fout fout f3

ensureAlive :: Int -> LTG()
ensureAlive f = do
  zeroDead <- isDead True f
  when zeroDead (revive f >> return ())
  zeroDead' <- isDead True f
  when zeroDead' $ lerror "ZeroDead"

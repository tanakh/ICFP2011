{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
import Control.Applicative
import qualified Control.Exception.Control as E
import Control.Monad
--import Control.Monad.State

import Data.Maybe


import LTG 

infix 1 $<<
($<<) :: Int -> Card -> LTG()
f $<< card = do
  ensureAlive f
  f $< card
  
infix 1 $>>
($>>) :: Card -> Int -> LTG()
card $>> f = do
  ensureAlive f
  card $> f
  
clearR :: Int -> LTG()
clearR ix = do
  f <- getField True ix
  when (f /= VFun "I") $ do
    Put $>> ix

applyR0 :: Int -> LTG ()
applyR0 fld = do
  K $>> fld
  S $>> fld
  fld $<< Get
  ensureAlive 0
  fld $<< Zero

numCost :: Int -> Int
numCost 0 = 1
numCost i = (i `mod` 2) + (numCost $ i `div` 2)


yumaf1 :: Int
yumaf1 = 8

yumaf2 :: Int
yumaf2 = 4

yumaf3 :: Int
yumaf3 = 3

yumafsave :: Int
yumafsave = 2

yumaCall :: Bool -> LTG()
yumaCall saveF0 = do
  enemy0Dead <- isDead False 0
  if enemy0Dead then return () -- no need to harrass
    else do
    -- 8671 = 255 * 32 + 255 * 2 + 1
    enoughs <- filterM (\i -> do 
                           x <- getVital True i
                           return (x > 8671)) $ [11..255] 
    let vs = [(numCost e, e) | e <- enoughs]
    let use = snd $ minimum vs

    -- Peroform attack
    when saveF0 $ do copyTo yumafsave 0
    copyTo use yumaf2
    num 0 yumaf1
    ensureAlive use
    use $< Zero
    when saveF0 $ do copyTo 0 yumafsave

createYuma :: LTG()
createYuma = do
-- v[f1] <- Yuma expression
-- v[f2] <- Yuma starter
--   1. copy to anywhere v[x] <- v[f2]
--   2. num 0 f1
--   3. v[x] $< Zero
  let f1 = yumaf1
  let f2 = yumaf2
  let f3 = yumaf3

  -- S (K (S (S Attack (K 255)) (K 8192))) Get

  -- v[f1] <- S Attack (K 255)
  clearR f1
  f1 $<< S
  f1 $<< Attack
  num 0 255
  K $>> 0
  applyR0 f1
  0 $<< Put

  -- v[f1] <- S (S Attack (K 255)) (K 255*2)
  S   $>> f1
  Dbl $>> 0
  K   $>> 0
  applyR0 f1
  0 $<< Put

  -- v[f3] <- S (S Help I) (K 255*32)
  Dbl $>> 0
  Dbl $>> 0
  Dbl $>> 0
  Dbl $>> 0
  K $>> 0
  clearR f3
  f3 $<< S
  f3 $<< Help
  f3 $<< I
  S  $>> f3
  applyR0 f3

  -- v[f3] <- (S v[f3] (S Get I))
  clearR 0
  0 $<< S
  0 $<< Get
  0 $<< I
  S $>> f3
  applyR0 f3

  -- S v[f1] v[f3]
  S $>> f1
  copyTo 0 f3
  applyR0 f1

  -- copyTo 0 f1

  copyTo f2 f1
  K $>> f2
  S $>> f2
  f2 $<< Get

kyoukoF1 :: Int
kyoukoF1 = 9

kyoukoF2 :: Int
kyoukoF2 = 6

kyoukoF4 :: Int
kyoukoF4 = 5

kyoukoF8 :: Int
kyoukoF8 = 7

kyoukoSave :: Int
kyoukoSave = 10

kyoukoAnAn :: Int -> LTG ()
kyoukoAnAn target = do
  let f1 = kyoukoF1 
  let f2 = kyoukoF2
  let f4 = kyoukoF4 
  let f8 = kyoukoF8
-- f1, f2: temp
-- f4
-- target: zombie target

  yumaCall False

  -- v[f4] <- S (S Help I) (lazyApply Copy f8)
  -- S (S Help I)

  -- v[f2] <- (lazyApply Copy f8)
  -- num: gen f8
  clear f2
  f2 $< Copy     ; yumaCall False
  num 0 f8
  lazyApply f2 0
  0 $< Put -- Just caching f2 to slot 0 (and used in copyTo 0 "f2")

  yumaCall False

  -- v[f2] <- S (S Help I) v[f8]; loop body
  --        = S (\x -> Help x x) (lazyApply Copy f8)
  -- num: kill f4
  copyTo 0 f2

  clear f2
  f2 $< S
  f2 $< Help
  f2 $< I
  S $> f2
  apply0 f2 -- S (S Help I) (S (K copy) (K 8))
  
  yumaCall False

  -- v[f4] <- S (lazyApply Copy f2) Succ; loop next
  -- num: gen f2
  clear f4
  f4 $< Copy
  num 0 f2
  lazyApply f4 0
  0 $< Put -- Cache (used in copyTo 0 "f4")
  S $> f4
  f4 $< Succ
          
  yumaCall False

  -- v[f2] <- S f2 f4
  -- num: kill f4
  S  $> f2
  copyTo 0 f4
  apply0 f2
  
  yumaCall False

  -- v[f1] <- S (K v[f2]) (lazyApply Copy f4); zombie
  -- v[0] = v[f4] = (lazyApply Copy f4)
  clear f4
  f4 $< Copy
  num 0 f4
  lazyApply f4 0
  0 $< Put;       yumaCall False
  copyTo 0 f4

  -- use f2 but no help
  copyTo f1 f2
  K $> f1
  S $> f1
  apply0 f1
  yumaCall False

  num f4 (255-target)
  Zombie $> f4

  lazyApply f4 f1

  yumaCall False

  copyTo 0 f4
  zombieOuterLoop target

getFirstWorthEnemy :: Int -> LTG (Maybe Int)
getFirstWorthEnemy dmg = do
  alives <- filterM 
            (\ix -> do
                al <- isAlive False ix
                vt <- getVital False ix
                return (al && vt >= dmg))
            [0..255]
  return $ listToMaybe alives

getAnySlot :: LTG (Maybe Int)
getAnySlot = do
  aliveidents <- filterM 
            (\ix -> do
                al <- isAlive True ix
                fn <- getField True ix
                return (al && fn == VFun "I"))
            [0..255]
  return $ listToMaybe aliveidents

ensureZombieDead :: Int -> LTG ()
ensureZombieDead target = do
  zombieReady <- isDead False target
  if zombieReady 
    then do
    return ()
    else do -- oops! They revived 255!
    vit <- getVital False target
    aliveslot <- getAnySlot
    case (vit, aliveslot) of
      (1, Just slot) -> do
        -- dec 
        num slot (255 - target)
        Dec $> slot
        ensureZombieDead target
      (1, Nothing) -> do
        return ()
      (x, _) -> do
        yumaCall True

-- get 3 * 2^n or 2^n smaller than x
getEasyInt :: Int -> Int
getEasyInt x | (x <= 3) = x
getEasyInt x = 
  max (head $ filter (\y -> y * 2 > x) twos) (head $ filter (\y -> y * 2 > x) threep)
  where
    twos = map (2^) [(0::Int)..]
    threep = 1 : map (\n -> 3*(2^n)) [(0::Int)..]

getMaxEnemy :: LTG Int
getMaxEnemy = do
  oppAlives <- filterM (isAlive False) [0..255]
  vitals <- mapM (getVital False) oppAlives
  return $ maximum vitals

zombieOuterLoop :: Int -> LTG ()
zombieOuterLoop target = do
  damage <- getEasyInt <$> getMaxEnemy
  lprint $ "damage " ++ show damage
  copyTo kyoukoSave 0 
  yumaCall False
  num kyoukoF8 damage
  copyTo 0 kyoukoSave
  zombieLoop damage target

zombieLoop :: Int -> Int -> LTG ()
zombieLoop dmg target = do
  elms <- getFirstWorthEnemy dmg
  case elms of
    Nothing -> do
      zombieOuterLoop target
    Just n -> do
      yumaCall True
      num kyoukoF4 n
      copyTo kyoukoF1 0
      ensureZombieDead target
      kyoukoF1 $< I
      zombieLoop dmg target

main :: IO ()
main = runLTG $ do
  forever $ do
    mb <- E.try createYuma
    case mb of
      Left (LTGError e) -> do
        return ()
      Right _ -> do
        return ()
    forever $ do        
      mb2 <- E.try $ do
        yumaCall False
        kyoukoAnAn 0
      case mb2 of
        Left (LTGError e) -> do
          return ()
        Right _ -> do
          return ()


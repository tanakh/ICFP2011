{-# LANGUAGE CPP #-}
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
    threep = 1 : map (\n -> 3*(2^n)) [(0::Int)..]


#ifdef KAMIJO
debugTag::String    -- Iize, omae ga mahou wo Junbi Shiteruttenarayo,
debugTag = "kamijo" -- Mazuhasonofuzaketagensouwobuchikowass!!

speedo :: Int -> Double
speedo x
    | x == 0 = 0
    | odd  x = 1 + speedo (x-1)
    | even x = 1 + speedo (div x 2)

getMaxEnemy :: LTG Int
getMaxEnemy = do
  oppAlives <- filterM (isAlive False) [0..255]
  vitals <- mapM (getVital False) oppAlives
  let targets = zip oppAlives vitals
      umami (i, v) = (fromIntegral v * 2 ** (0-speedo i) , v)
  return $ snd $ maximum $ map umami targets 
#else
debugTag::String
debugTag = "kyoko"

getMaxEnemy :: LTG Int
getMaxEnemy = do
  oppAlives <- filterM (isAlive False) [0..255]
  vitals <- mapM (getVital False) oppAlives
  return $ maximum vitals
#endif


zombiePort :: Int
zombiePort = 255


kyoukoMain :: LTG()
kyoukoMain = do
  dmg <- getEasyInt <$> getMaxEnemy
  zombifySlotVital <- getVital False zombiePort
  let zombifySlotV = getEasyInt zombifySlotVital
  alives <- filterM (\x -> do 
                        v <- getVital True x
                        return $ v > zombifySlotV)
            [1..255]
  -- TODO: raise error to increase vitality
  if length alives < 2 
    then return ()
    else do
    attack2 (alives !! 0) (alives !! 1) 0 zombifySlotV
{-
    attack      (alives !! 0) 0 zombifySlotV
    attack      (alives !! 1) 0 zombifySlotV
-}

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

    num 6 dmg
    kyokoAnAn 1 2 3 4 5 7 zombiePort dmg

--  attackFA    1 2 18 3 5 6 8192
--  attackLoopFA 1 2 18 5 0 0
--  sittingDuck
  
main :: IO ()
main = runLTG $ do
  lprint debugTag

  let usedports = [0,1,2,3,4,6,7]

  forever $ do
    ds <- filterM (isDead True) $ (filter (`elem` usedports))[0..255]
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


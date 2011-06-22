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
numCost i = 1 + (i `mod` 2) + (numCost $ i `div` 2)


yumaf1 :: Int
yumaf1 = 1

yumaf2 :: Int
yumaf2 = 2

yumaf3 :: Int
yumaf3 = 3

yumaf4 :: Int
yumaf4 = 4

yumaf5 :: Int
yumaf5 = 5

yumafsave :: Int
yumafsave = 2


createYuma :: LTG()
createYuma = do
-- v[f1] <- Yuma expression
-- v[f2] <- Yuma starter
--   1. copy to anywhere v[x] <- v[f2]
--   2. num 0 f1
--   3. v[x] $< Zero
-- v[f4] <- S (K (zombie 255)) (S (K dec) (K 255))
--   1. copy to anywhere v[x] <- v[f4]
--   2. v[x] $< anything

-- S (K v[f1]) v[f2]

  let f1 = yumaf1
  let f2 = yumaf2
  let f3 = yumaf3
  let f4 = yumaf4
  let f5 = yumaf5

  3      $<< Zero
  Succ   $>> 3
  4      $<< Zero
  Succ   $>> 4
  Succ   $>> 4
  --5      $<< Zero
  --Succ   $>> 5
  --Succ   $>> 5
  --Succ   $>> 5
  Attack $>> 3
  Attack $>> 4
  --Attack $>> 5
  -- 7+5

  0    $<< Zero
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  Dbl  $>> 0
  Succ $>> 0
  -- v[0] = 255; 16

  applyR0 3
  applyR0 4
  --applyR0 5
  -- 8 + 4

  -- S (S Zombie) (K 255)
  1    $<< S
  1    $<< Zombie
  S    $>> 1
  K    $>> 0
  applyR0 1
  0    $<< Put

  Dbl  $>> 0
  Dbl  $>> 0
  Dbl  $>> 0
  Dbl  $>> 0
  Dbl  $>> 0
  -- 5

  applyR0 3
  applyR0 4
  --applyR0 5
  -- 8 + 4
  copyTo 0 1

{-
\x -> f (Dbl x)
S (K (S (K f) Dbl) Dbl

  S f (K 255) x = (f x) 255 = Zombie 255 hoge
  f x y = Zombie y x
  S f g x = (f 255) (g 255)
  S Zombie any
  S (S Zombie) (K 255)
  S (S Zombie) (K 255) x
  -> S Zombie x 255
  -> Zombie 255 (x 255)
-}


-- YumaExpression:                              S (S(S(attack)(K(255)))(K(I))) (S(S(S(help)(I))(K(I)))(S(get)(I)))
-- YumaStarter: S (K YumaExpression) Get        S (K(S(S(S(attack)(K(255)))(K(I)))(S(S(S(help)(I))(K(I)))(S(get)(I))))) (get)
-- Yuma2:                                       S (K(zombie(255))) (S(K(dec))(K(255)))

-- 1={10000,S(S(S(attack)(K(255)))(K(510)))(S(S(S(help)(I))(K(8160)))(S(get)(I)))}
-- 2={10000,S(K(S(S(S(attack)(K(255)))(K(510)))(S(S(S(help)(I))(K(8160)))(S(get)(I)))))(get)}
-- 4={10000,S(K(zombie(255)))(S(K(dec))(K(255)))}


isYumaStarter x = case x of
  (VApp (VApp (VFun "S") _) (VFun "get")) -> True
  _ -> False

isYuma2 x = not (isYumaStarter x) && case x of
--  (VApp (VApp (VFun "S") _) (VApp (VFun "zombie") _)) -> True
  (VApp (VApp (VFun "S") _) (VApp (VFun "K") _)) -> True
--  (VApp (VApp (VFun "S") (VApp (VFun "K") _) ) _) -> True
  _ -> False

-- S(S(I)(K(255)))(zombie(255))

isYumaExpression x = not (isYumaStarter x) && not (isYuma2 x) && case x of
  (VApp (VApp (VFun "S") _) _) -> True
  _ -> False

find :: (Value -> Bool) -> Int -> Int -> Int -> LTG ((Int, Int), Int)
find f minNum minVital a = do -- a > 0: max numCost / a < 0: min numCost
  l <- filterM (\i -> do
                   field <- getField True i
                   v     <- getVital True i
                   return (v >= minVital && (f field)) ) $ [0..255] 
  if length l >= minNum
    then do
      let vs  = [(a * (numCost i), i) | i <- l]
      return ((maximum vs),length l)
    else return ((-1,-1),length l)


{-

copy, wishToSummon

copy, !wishToSummon

-}

summonOrCopy f name summon ixToCopy copy minNumToSummon minVitalToSummon summonScore wishToSummon canSummon l0 = do
  -- summon
  ((_,ix),_) <- find f minNumToSummon minVitalToSummon 1
  let l1 = if ix >= 0
             then
               if canSummon then (summonScore, "Action: summon " ++ name, (summon ix)):l0  -- OK, Yuma2 found!
               else l0
             else l0

  -- copy Yuma2
  ((cost,ix2),len) <- find f 1 1 (-1)
  let l2 = if ix2 >= 0 && ixToCopy >= 0
             then ((if wishToSummon then 256 else 0) - len * 512 + cost,  "Action: copy " ++ name, (copy ix2)):l1
             else l1

  return l2

mamimami = do
  f0 <- getField False 0
  v0 <- getVital False 0



  lstToKill <- mapM (\i -> do
                   v     <- getVital False i
                   return ((if v > 0 then v else 100000) * (1 + numCost (255-i)),i) ) $ [0..255] 
  let (vToKill,iToKill0) = (minimum lstToKill)
  let iToKill = if vToKill < 100000 then 255 - iToKill0 else 0

  liveList <- mapM (\i -> do
                   v     <- getVital True i
                   return ((v, -(numCost i)), i)) $ ([8..153]++[155..255])
  let ixLive = if length liveList >= 1
                 then snd $ maximum liveList
                 else 154

  toCopyList <- filterM (\(_,i) -> do
                   f     <- getField True i
                   return (not (isYumaStarter f) && not (isYuma2 f) && not (isYumaExpression f))) $ liveList
  let ixToCopy = if length toCopyList >= 1
                 then snd $ maximum toCopyList
                 else (-1)

  let wishToSummonYuma2         = (v0 <= 0 && f0 /= VFun "I") || v0 >= 1

  l0 <- summonOrCopy isYuma2       "Yuma2"       (\ix -> (\_ -> do ix $<< Dec )) ixToCopy (\ix -> (\_ -> do copyTo ixToCopy ix)) 2 1           1000 wishToSummonYuma2        wishToSummonYuma2       l0

  let iter l maxv maxname maxf = case l of
                          []       -> return (maxf, maxname)
                          (v,name,f):tl -> do
                                             if v > maxv then iter tl v name f else iter tl maxv maxname maxf


  (maxf, maxname) <- (iter l0 (-2000000000) "Idle" (\_ -> do
                                                            revive ixLive
                                                            num ixLive iToKill
                                                            Dec $> ixLive
                                                   ))
  lprint (maxname ++ "\n")
  maxf ()


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

sittingDuck = do
  I $> 0
  sittingDuck

main :: IO ()
main = runLTG $ do
  forever $ do
    lprint "0"
    mb <- E.try createYuma
    case mb of
      Left (LTGError e) -> do
        lprint "A"
        return ()
      Right _ -> do
        lprint "B"
        return ()
    forever $ do        
      mb2 <- E.try $ do
        mamimami
      case mb2 of
        Left (LTGError e) -> do
          lprint "D"
          return ()
        Right _ -> do
          lprint "E"
          return ()


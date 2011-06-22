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
yumaf1 = 2

yumaf2 :: Int
yumaf2 = 1

yumaf3 :: Int
yumaf3 = 3

yumaf4 :: Int
yumaf4 = 5

yumaf5 :: Int
yumaf5 = 4

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

  -- S (K (S (S Attack (K 255)) (K 8192))) Get

  -- v[f1] <- S Attack (K 255)
  clearR f1
  f1 $<< S
  f1 $<< Attack
  num 0 255
  K $>> 0
  applyR0 f1

  -- v[f5] <- S (K dec) (K 255)
  clearR f5
  f5 $<< K
  f5 $<< Dec
  S  $>> f5
  applyR0 f5

  0 $<< Put

  -- v[f4] <- (zombie 255)
  {-
  clearR f4
  f4 $<< Zombie
  applyR0 f4
  K   $>> f4
  S   $>> f4
  -}
  copyTo f4 0

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

  copyTo 153 f2
  num 0 f1
  153 $<< Zero

  Zombie $>> f4
  K $>> f4
  S $>> f4
  copyTo 0 f5
  applyR0 f4

  Put $>> f5
  Put $>> f3
  Put $>> 0

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
  (VApp (VApp (VFun "S") (VApp (VFun "K") _) ) _) -> True
  _ -> False

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
                 else 8

  toCopyList <- filterM (\(_,i) -> do
                   f     <- getField True i
                   return (not (isYumaStarter f) && not (isYuma2 f) && not (isYumaExpression f))) $ liveList
  let ixToCopy = if length toCopyList >= 1
                 then snd $ maximum toCopyList
                 else (-1)

  let wishToSummonYuma2         = (v0 <= 0 && f0 /= VFun "I") || v0 == 1
  let wishToSummonYumaStarter0  = v0 > 0

  (wishToSummonYumaStarter,l0) <- do
    myf0 <- getField True 0 -- check v[0] is OK and v[v[0]] is YumaExpression
    myv0 <- getVital True 0 -- check v[0] is OK and v[v[0]] is YumaExpression
    ((_,ixYumaStarter),_) <- find isYumaStarter 2 (8192+512+1) 1
    if ixYumaStarter < 0
      then return (False, [])
      else if myv0 >= 0 then do
        f0ready <- case myf0 of
          VInt iy | 0 <= iy && iy <= 255 -> do
            myfiy <- getField True iy
            return (isYumaExpression myfiy)
          _ -> return False
        if f0ready
          then return (wishToSummonYumaStarter0,[])
          else do  -- live, but value not set
            ((_,ixYumaExpression),_) <- find isYumaExpression 1 1 (-1)
            if ixYumaExpression >= 0
              then return (False,[(if wishToSummonYumaStarter0 && not wishToSummonYuma2 then 800 else (1 - 512 * 2), "Action: v[0] <- ixYumaExpression", (\_ -> do numR 0 ixYumaExpression))]) -- copy YumaExpr.
              -- ÅüÅü"Action: v[0] <- ixYumaExpression"
              else return (False,[])
      else do -- v[0] is dead
        return (False, [(if wishToSummonYumaStarter0 && not wishToSummonYuma2 then 800 else (1 - 512 * 2), "Action: Revive 0", (\_ -> (do
                                clearR ixLive
                                ixLive $<< Zero
                                Revive $>> ixLive
                             )))])
              -- ÅüÅü"Action: Revive v[0]"

  l0 <- summonOrCopy isYuma2       "Yuma2"       (\ix -> (\_ -> do ix $< Put )) ixToCopy (\ix -> (\_ -> do copyTo ixToCopy ix)) 2 1           1000 wishToSummonYuma2        wishToSummonYuma2       l0
  l0 <- summonOrCopy isYumaStarter "YumaStarter" (\ix -> (\_ -> do ix $< Zero)) ixToCopy (\ix -> (\_ -> do copyTo ixToCopy ix)) 2 (8192+512+1) 900 wishToSummonYumaStarter0 wishToSummonYumaStarter l0

  let iter l maxv maxname maxf = case l of
                          []       -> return (maxf, maxname)
                          (v,name,f):tl -> do
                                             {-
                                             lprint name
                                             lprint " with cost "
                                             lprint v
                                             lprint "\n"
                                             -}
                                             if v > maxv then iter tl v name f else iter tl maxv maxname maxf


  (maxf, maxname) <- (iter l0 (-2000000000) "Idle" (\_ -> do
                                                            num 154 iToKill
                                                            Dec $> 154
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
    mb <- E.try createYuma
    case mb of
      Left (LTGError e) -> do
        return ()
      Right _ -> do
        return ()
    forever $ do        
      mb2 <- E.try $ do
        mamimami
      case mb2 of
        Left (LTGError e) -> do
          return ()
        Right _ -> do
          return ()


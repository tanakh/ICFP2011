#!/usr/bin/env runhaskell
import LTG

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.Vector ((!))
import qualified Data.Vector as V
import System.IO.Unsafe
import System.Random


attackN, attackDmg, helpN :: Int
attackN   = 11112
attackDmg = 10000
helpN     =  9999

i1, i2, i3, i4, i255 :: Int
(i1: i2: i3: i4: i5: i6: _) = [1..]
i255 = 255


openingSwitch :: IORef Bool
openingSwitch = unsafePerformIO $ newIORef True
{-# NOINLINE openingSwitch #-}

isOpening :: LTG Bool
isOpening = liftIO $ readIORef openingSwitch
{-# NOINLINE isOpening #-}

main :: IO ()
main = runLTGs
       [(isOpening                   , openingMove ),
        (mamitta                     , bikunbikun  ),
        (enemyWeak                   , madanNoButou),
        (isAlive False 0             , discipline  ),
        ((<55000) <$> getVital True 0, pumpUp      ),
        (return True                 , shoot       )
       ]


infix 1 $<<
($<<) :: Int -> Card -> LTG()
i $<< card = do
  ensureAlive i
  i $< card
  
infix 1 $>>
($>>) :: Card -> Int -> LTG()
card $>> i = do
  ensureAlive i
  card $> i
  
clearR :: Int -> LTG()
clearR ix = do
  f <- getField True ix
  when (f /= VFun "I") $ do
    Put $>> ix

applyR0 :: Int -> LTG ()
applyR0 i = do
  K $>> i
  S $>> i
  i $<< Get
  ensureAlive 0
  i $<< Zero

numCost :: Int -> Int
numCost 0 = 1
numCost i = 1 + (i `mod` 2) + (numCost $ i `div` 2)

-- get 3 * 2^n or 2^n smaller than x
getEasyInt :: Int -> Int
getEasyInt x | (x <= 3) = x
getEasyInt x = 
  max (head $ filter (\y -> y * 2 > x) twos) (head $ filter (\y -> y * 2 > x) threep)
  where
    twos = map (2^) [(0::Int)..]
    threep = 1 : map (\n -> 3*(2^n)) [(0::Int)..]


{- Ikki ni Kimesasete
   Morauwayo ! -}
openingMove :: LTG ()
openingMove = do
  i2     $<< Zero
  i3     $<< Zero
  Succ   $>> i3
  Attack $>> i2
  Attack $>> i3
  -- 5

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

  applyR0 i2
  applyR0 i3
  --applyR0 5
  -- 8 

  -- S (S Dec) (K 255)
  i1   $<< S
  i1   $<< Dec
  S    $>> i1
  K    $>> 0
  applyR0 i1
  0    $<< Put

  Dbl  $>> 0
  Dbl  $>> 0
  Dbl  $>> 0
  Dbl  $>> 0
  Dbl  $>> 0
  -- 5
  copyTo i255 i1
  applyR0 i2
  applyR0 i3
  --applyR0 5
  -- 8 + 4

  liftIO $ writeIORef openingSwitch False

  forever nop


{- Otonashiku shite ireba
   Kaeri ni chanto kaihou shiteageru. -}
discipline :: LTG ()
discipline = do
  forever $ do
    ensureAlive i1
    --lprint "Kora <3"
    i1 $<< Get

{- Oshikattawane! Tiro - - - -}
pumpUp :: LTG ()
pumpUp =
  forever $ do
    (v, _, i) <- maximum <$> mapM f [1..255]
    if v > helpN
    then do
      ensureAlive i4
      num i4 i
      Help $>> i4
      i4 $<< Zero
      ensureAlive i3
      num i3 helpN
      copyTo 0 i3

      debug
      applyR0 i4
    else do
      ensureAlive i4
      num i4 0
      Help $>> i4
      i4 $<< Zero
      v0 <- getVital True 0
      num 0 (getEasyInt v0)

      debug
      applyR0 i4
        where        
          f i = do
              v <- getVital True i
              return (v, -numCost i, i)

{- - - - Finale! -}
shoot :: LTG ()
shoot =
  forever $ do
    (v, _, i) <- maximum <$> mapM f [1..255]
    if v >= attackDmg
    then do
      ensureAlive i4
      num i4 0
      Attack $>> i4
      num 0 (255-i)
      applyR0 i4
      ensureAlive i2
      num i2 attackN
      copyTo 0 i2

      debug
      applyR0 i4
    else do
      ensureAlive i4
      i4 $<< Zero
      Attack $>> i4
      num 0 (255-i)
      applyR0 i4
      ve <- getVital False i
      num 0 (getEasyInt (2*ve))

      f4 <- getField True i4 
      f0 <- getField True 0
      debug
      applyR0 i4
        where        
          f i = do
              v <- getVital False i
              return (v, numCost i, i)


{- Atoha zako bakkarine! -}
enemyWeakSwitch :: IORef Bool
enemyWeakSwitch = unsafePerformIO $ newIORef False
{-# NOINLINE enemyWeakSwitch #-}

enemyWeak :: LTG Bool
enemyWeak = do
  sw <- liftIO $ readIORef enemyWeakSwitch 
  if sw then return True
  else do
     enemyVs <- mapM (getVital False) [0..255]
     let ret = (maximum enemyVs <= 2) 
     liftIO $ writeIORef enemyWeakSwitch ret
     return ret


{- Sokkou de kimesasete morauwayo! -}
madanNoButou :: LTG()
madanNoButou = forever $ do
  let weapon = VApp (VApp (VFun "S") (VFun "dec")) (VApp (VApp (VFun "S") (VApp (VApp (VFun "S") (VApp (VFun "K") (VFun "get"))) (VApp (VFun "K") (VInt 1)))) (VFun "succ"))
  f <- getField True i5
  if (f/=weapon) then do
                   lprint "buki tsukuruyo"
                   clear 0 >> prepareMagicalBullet i5 i6
  else do
    lprint "utuyo"
    alives <- fmap V.fromList $ mapM (isAlive False) [0..255] 
    vitals <- fmap V.fromList $ mapM (getVital False) [0..255] 
    let range i = [i .. endPoint i]
        endPoint i = min 255 (i+124)
        deaths  i = sum [if alives ! j && vitals ! j == 1 then 1 else 0  | j<-range i]
        damages i = sum [if alives ! j && vitals ! j >= 1 then 1 else 0  | j<-range i]
        withScore i = ((deaths i, damages i,numCost (endPoint i)), i)
        target =  snd $ maximum $ map withScore [0..255]
    lprint $ "madan at " ++ show target
    summonMami i5 i6 target
  


debug = do
  f4 <- getField True i4 
  f0 <- getField True 0
  lprint $ show f4 ++ " $ "  ++ show f0



mamitta :: LTG Bool
mamitta = isOpening

{- Tasukete ...  -}
bikunbikun :: LTG ()
bikunbikun = 
  forever $do
    deadEvens  <- filterM (isDead  True) [0,2..128]
    aliveEvens <- filterM (isAlive True) [0,2..128]
    if null deadEvens
    then do
      nop
    else do
      _ <- revive (head deadEvens)
      return ()

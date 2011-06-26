{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
import Control.Applicative
import qualified Control.Exception.Control as E
import Control.Monad
import Control.Monad.State

import Data.Maybe
import Data.List

import LTG 

fai1  = 1
fai2  = 2
faffa = 3

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
        killTarget target

zombieLoop :: Int -> Int -> Int -> Int -> Int -> LTG ()
zombieLoop f5 f4 f1 dmg target = do
  elms <- getFirstWorthEnemy dmg
  case elms of
    Nothing -> return ()
    Just n -> do
      num f4 n
      copyTo f1 f5
      ensureZombieDead target
      f1 $< I
      zombieLoop f5 f4 f1 dmg target

ofN :: Int -> Value
ofN x  = VInt x

ofC :: Card -> Value
ofC x = VFun (cardName x)

infixl 1 $|
($|) :: Value -> Value -> Value
x $| y  = VApp x y

lazyApplyIntermediate :: Value -> Value -> Value
lazyApplyIntermediate f g =
  -- S (K f) (K g)
  (ofC S) $| (ofC K $| f) $| (ofC K $| g)

makeFieldsUnlessConstructed :: [(Int, Value)] -> LTG() -> LTG()
makeFieldsUnlessConstructed pairs procedure = do
  ffs <- mapM
         (\(f, v) -> do
             k <- getField True f
             return (k == v))
         pairs
  if and ffs 
    then do 
    lprint $ "Reusing " ++ show (map fst pairs)
    return ()
    else do
    procedure

makeFieldUnlessConstructed :: Int -> Value -> LTG() -> LTG()
makeFieldUnlessConstructed f lval procedure = do
  makeFieldsUnlessConstructed [(f, lval)] procedure

kyokoAnAnFA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
kyokoAnAnFA i1 i2 ffa f1 f2 f4 f8 target dmg = do
-- f1, f2: temp
-- f4
-- target: zombie target

      num f8 dmg

      clear f2
      f2 $< Copy
      num i2 f8
      lazyApplyFA i1 i2 ffa f2 f2 i2
      i2 $< Put

      -- v[f2] <- S (S Help I) v[f8]; loop body
      --        = S (\x -> Help x x) (lazyApply Copy f8)
      -- num: kill f4
      copyTo i2 f2

      clear f2
      f2 $< S
      f2 $< Help
      f2 $< I
      S $> f2
      applyFA i1 i2 ffa f2 f2 i2 -- S (S Help I) (S (K copy) (K 8))

      -- v[f4] <- S (lazyApply Copy f2) Succ; loop next
      -- num: gen f2
      clear f4
      f4 $< Copy
      num i2 f2
      lazyApplyFA i1 i2 ffa f4 f4 i2
      i2 $< Put
      S $> f4
      f4 $< Succ

      -- v[f2] <- S f2 f4
      -- num: kill f4
      S  $> f2
      copyTo i2 f4
      applyFA i1 i2 ffa f2 f2 i2

  -- v[f1] <- S (K v[f2]) (lazyApply Copy f4); zombie
      -- v[0] = v[f4] = (lazyApply Copy f4)
      clear f4
      f4 $< Copy
      num i2 f4
      lazyApplyFA i1 i2 ffa f4 f4 i2
      i2 $< Put
      copyTo i2 f4

      -- use f2 but no help
      copyTo f1 f2
      K $> f1
      S $> f1
      applyFA i1 i2 ffa f1 f1 i2

      num f4 (255-target)
      Zombie $> f4

      lazyApplyFA i1 i2 ffa f4 f4 f1
      copyTo 16 f4

      zombieLoop 16 f4 f1 dmg target

sittingDuck :: LTG()
sittingDuck = do
  I $> 0
  sittingDuck

-- get 3 * 2^n or 2^n smaller than x
getEasyInt :: Int -> Int
getEasyInt x | (x <= 3) = x
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

checkTarget :: Int -> LTG ()
checkTarget target = do
  isTargetAlive <- isAlive False target
  when isTargetAlive $ lerror "Not dead"

killTarget :: Int -> LTG()
killTarget target = do
  zombifySlotVital <- getVital False target
  let zombifySlotV = getEasyInt zombifySlotVital
  alives <- filterM (\x -> do 
                        v <- getVital True x
                        return $ v > zombifySlotV)
            [1..255]

    -- dmg > 2 -> attack is issued
    -- Create wall between the cut, to control damage, if possible
  case length alives of
    n | n < 2 -> lerror "there are no vital"
    n | zombifySlotV > 1 && n >= 5 ->  attack2FA fai1 fai2 faffa (alives !! 1) (alives !! 4) (255 - target) zombifySlotV
    _ | zombifySlotV > 1  ->  attack2FA fai1 fai2 faffa (alives !! 0) (alives !! 1) (255 - target) zombifySlotV
    _ -> return ()
  when (zombifySlotV > 1) $ checkTarget target 

chooseTarget :: LTG Int
chooseTarget = do
  vs <- forM [255,254..240] $ \i -> do
    vit <- getVital False i
    return (vit, -i)
  return $ negate $ snd $ minimum vs

kyoukoMain :: LTG()
kyoukoMain = do
  target <- chooseTarget
  dmg <- getEasyInt <$> getMaxEnemy

  isTargetAlive <- isAlive False target
  when isTargetAlive $ killTarget target
  kyokoAnAnFA fai1 fai2 faffa 4 6 8 5 target dmg

keepAlive :: Int -> LTG ()
keepAlive ix = do
  d <- isDead True ix
  when d $ do
    _ <- revive ix
    keepAlive ix

ignExc :: LTG a -> LTG ()
ignExc m = do
  mb <- E.try m
  case mb of
    Left (LTGError _) -> return ()
    Right _ -> return ()

yose :: LTG ()
yose = do
  forever $ ignExc $ do
    keepAlive 0
    num 0 0
    forM_ [(1::Int)..255] $ \_ -> do
      keepAlive 0
      keepAlive 1
      copyTo 1 0
      Dec $> 1
      Succ $> 0

waruagaki :: LTG ()
waruagaki = do
  keepAlive 0
  keepAlive 1
  keepAlive 2
  num 0 1
  Inc $> 0
  num 0 2
  Inc $> 0

speedo :: Int -> Int
speedo x
    | x == 0 = 0
    | odd  x = 1 + speedo (x-1)
    | even x = 1 + speedo (div x 2)

main :: IO ()
main = runLTG $ do

  futureApply fai1 fai2 faffa 4

  let range = 10
  let necks = take (range) $ map snd $ sort $[(speedo i, i) | i<-[1..255]]
  forever $ do
    ds <- filterM (isDead True) necks
    if null ds
      then do
      turn <- getTurnCnt
      if turn >= 100000 - 1536
        then do
        lprint "yose mode"
        yose
        else do
        lprint "normal mode"
        mb <- E.try kyoukoMain
        case mb of
          Left (LTGError e) -> do
            case e of
              "there are no vital" -> do
                lprint "waruagaki mode"
                waruagaki
              _ -> do
                lprint e
            return ()
          Right _ -> do
            return ()
        return ()
      else do
      rankedTgt <- mapM rankDeads ds
      let reviveTgt = snd $ head $ sort rankedTgt          
      lprint $ "Revive mode: " ++ show (sort rankedTgt)
      ignExc $ revive reviveTgt
      lprint "Revive done"
      return ()

rankDeads :: Int -> LTG (Int, Int)
rankDeads i 
    | i == 0 = return (0, i)
    | True   = do
          fa <- isAlive True (i-1)
          return (if fa then 1 else 0, i)

--  futureApply 1 2 18 3


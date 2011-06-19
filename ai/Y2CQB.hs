{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
import Control.Applicative
import qualified Control.Exception.Control as E
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import System.Environment

import LTG 

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

ensureZombieDead :: LTG ()
ensureZombieDead = do
  zombieReady <- isDead False 255
  if zombieReady 
    then do
    return ()
    else do -- oops! They revived 255!
    vit <- getVital False 255
    aliveslot <- getAnySlot
    case (vit, aliveslot) of
      (1, Just slot) -> do
        -- dec 
        slot $< Zero
        Dec $> slot
        ensureZombieDead
      _ -> return ()

zombieLoop :: Int -> Int -> Int -> LTG ()
zombieLoop f4 f1 dmg = do
  --x0 <- getField True 0; lprint x0
  elms <- getFirstWorthEnemy dmg
  case elms of
    Nothing -> return ()
    Just n -> do
      num f4 n
      copyTo f1 0
      ensureZombieDead
      f1 $< I
      zombieLoop f4 f1 dmg


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

makeFieldUnlessConstructed :: Int -> Value -> LTG() -> LTG()
makeFieldUnlessConstructed f lval procedure = do
  ff <- getField True f
  if ff == lval 
    then do 
    -- lprint $ "Reusing " ++ show f
    return ()
    else do
    {-
    lprint $ "Failed reusing " ++
      show f ++ " [expected " ++ show lval ++ " but was " ++ show ff ++ "]"-}
    procedure

kyokoAnAn :: Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
kyokoAnAn f1 f2 f4 f8 target dmg = do
-- f1, f2: temp
-- f4
-- target: zombie target

  -- reuse field 0 to speed up
  ff0 <- getField True 0
  if ff0 == VInt dmg then do
    copyTo f8 0
    else do
    num f8 dmg

  makeFieldUnlessConstructed 0
    -- I know it's ugly
    (VApp (VApp (VFun "S") (VApp (VFun "K") (VApp (VFun "zombie") (VInt (255 - target))))) (VApp (VFun "K") (VApp (VApp (VFun "S") (VApp (VFun "K") (VApp (VApp (VFun "S") (VApp (VApp (VFun "S") (VApp (VApp (VFun "S") (VFun "help")) (VFun "I"))) (VApp (VApp (VFun "S") (VApp (VFun "K") (VFun "copy"))) (VApp (VFun "K") (VInt 2))))) (VApp (VApp (VFun "S") (VApp (VApp (VFun "S") (VApp (VFun "K") (VFun "copy"))) (VApp (VFun "K") (VInt 3)))) (VFun "succ"))))) (VApp (VApp (VFun "S") (VApp (VFun "K") (VFun "copy"))) (VApp (VFun "K") (VInt 4)))))) $ do
      -- v[f4] <- S (S Help I) (lazyApply Copy f8)
      -- S (S Help I)

      -- v[f2] <- (lazyApply Copy f8)
      -- num: gen f8
      clear f2
      f2 $< Copy
      num 0 f8
      lazyApply f2 0
      0 $< Put

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

      -- v[f4] <- S (lazyApply Copy f2) Succ; loop next
      -- num: gen f2
      clear f4
      f4 $< Copy
      num 0 f2
      lazyApply f4 0
      0 $< Put
      S $> f4
      f4 $< Succ

      -- v[f2] <- S f2 f4
      -- num: kill f4
      S  $> f2
      copyTo 0 f4
      apply0 f2

  -- v[f1] <- S (K v[f2]) (lazyApply Copy f4); zombie
      -- v[0] = v[f4] = (lazyApply Copy f4)
      clear f4
      f4 $< Copy
      num 0 f4
      lazyApply f4 0
      0 $< Put
      copyTo 0 f4

      -- use f2 but no help
      copyTo f1 f2
      K $> f1
      S $> f1
      apply0 f1

      num f4 (255-target)
      Zombie $> f4

      lazyApply f4 f1
      copyTo 0 f4

  zombieLoop f4 f1 dmg

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


kyoukoMain :: LTG()
kyoukoMain = do
  dmg <- getEasyInt <$> getMaxEnemy
  zombifySlotVital <- getVital False 255
  let zombifySlotV = getEasyInt zombifySlotVital
  alives <- filterM (\x -> do 
                        v <- getVital True x
                        return $ v > zombifySlotV)
            [1..255]

    -- dmg > 2 -> attack is issued
    -- "dec" is issued if dmg == 1 
    -- Create wall between the cut, to control damage, if possible
  case length alives of
    n | n < 2 -> lerror "there are no vital"
    n | zombifySlotV > 1 && n >= 5 ->  attack2 (alives !! 1) (alives !! 4) 0 zombifySlotV
    _ | zombifySlotV > 1  ->  attack2 (alives !! 0) (alives !! 1) 0 zombifySlotV
    _ -> return ()
  kyokoAnAn 1 3 4 2 255 dmg




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
    forM_ [(0::Int)..255] $ \_ -> do
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
  lprint debugTag

  (range:_) <- lift $ getArgs
  let necks = take (read range) $ map snd $ sort $[(speedo i, i) | i<-[0..255]]
  forever $ do
    ds <- filterM (isDead True) necks
    if null ds
      then do
      turn <- turnCnt <$> get
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
    | i == 0 = return (1, i)
    | True   = do
          fa <- isAlive True (i-1)
          return (if fa then 1 else 0, i)


--  futureApply 1 2 18 3


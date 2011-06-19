{-# Language BangPatterns #-}

module LTG.Simulator (
  Simulator(..),
  newSimulator,
  execStep,
  
  myState,
  oppState,
  printState,
  
  Hand,
  HandC,
  get13, get23, get33,
  State(..),
  Value(..),
  Monitor(..)
  ) where

import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import qualified Data.Vector.Mutable as MV
import System.IO
import System.IO.Unsafe

import LTG.Base (Card, nameToCard)
import LTG.Exception

get13 :: (a1,a2,a3)->a1
get13 (a1,a2,a3) = a1
get23 :: (a1,a2,a3)->a2
get23 (a1,a2,a3) = a2
get33 :: (a1,a2,a3)->a3
get33 (a1,a2,a3) = a3


data Simulator
  = Simulator
    { turnCnt :: !Int
    , phase   :: !Bool -- sente turn : True, gote turn : False
    , p1State :: State
    , p2State :: State
    }

newSimulator :: IO Simulator
newSimulator = do
  s1 <- newState
  s2 <- newState
  return $ Simulator 0 True s1 s2

type Hand  = (Int, Int, String)
type HandC = (Int, Int, Card)

myState :: Simulator -> State
myState Simulator { phase = True, p1State = stat } = stat
myState Simulator { p2State = stat } = stat

oppState :: Simulator -> State
oppState Simulator { phase = True, p2State = stat } = stat
oppState Simulator { p1State = stat } = stat

execStep :: Hand -> Simulator -> IO (Simulator, String)
execStep myHand@(typ, pos, name) s = do
  em <- allDead my   -- proponent in phase s
  eo <- allDead opp
  monAtPos <- MV.read (monitor my) pos
  MV.write (monitor my) pos $ monAtPos { propHandCount = 1 + propHandCount monAtPos }
  let myHandC = (typ, pos, nameToCard name)
      t = turnCnt s
  MV.write (history my) t myHandC 
  writeIORef (historyLength my) (t+1)

  case (em, eo) of
    (True, True) -> do
      return (s, "!! draw")
    (True, _) -> do
      return (s, "!! player" ++ show (if phase s then 1 else 0) ++ " win")
    (_, True) -> do
      return (s, "!! player" ++ show (if phase s then 0 else 1) ++ " win")
    _ | turnCnt s >= 100000 -> do
      return (s, "!! draw")
    _ -> do
      forM_ [0..255] $ \i -> do
        vi <- MV.read (vital my) i
        fi <- MV.read (field my) i
        when (vi == -1) $ do
          -- print "*** ZOMBIE ***"
          writeIORef gcnt 0
          es <- E.try $ eval True (VApp fi (VFun "I")) my opp
          case es of
            Left (E.SomeException e) -> do
              hPrint stderr e
            _ ->
              return ()
          MV.write (field my) i (VFun "I")
          MV.write (vital my) i 0
      
      let fn = getCard name
      sl <- MV.read (field my) pos
      
      let val = if typ==1 then VApp fn sl else VApp sl fn
  
      vi <- MV.read (vital my) pos
      if vi > 0
        then do
        writeIORef gcnt 0
        eres <- E.try $ eval False val my opp
        case eres of
          Left (E.SomeException e) -> do
            hPrint stderr e
            MV.write (field my) pos (VFun "I")
          Right res ->
            MV.write (field my) pos res
        else do
          MV.write (field my) pos (VFun "I")
      
      return (s { turnCnt = turnCnt s + (if phase s then 0 else 1)
                , phase = not (phase s) }, "")
  
  where
    my = myState s
    opp = oppState s

data Value
  = VInt Int
  | VFun String
  | VApp Value Value
  deriving (Eq, Show)

showValue :: Value -> String
showValue v = case v of
  VInt i -> show i
  VFun s -> s
  VApp x y -> showValue x ++ "(" ++ showValue y ++ ")"

data Monitor
  = Monitor 
    { propHandCount :: Int 
    , zombieSeedCount :: Int 
    }
  deriving (Eq, Show)

initialMonitor :: Monitor
initialMonitor = Monitor { propHandCount = 0, zombieSeedCount = 0}

data State
  = State
    { field :: MV.IOVector Value
    , vital :: MV.IOVector Int
    , monitor :: MV.IOVector Monitor
    , history :: MV.IOVector HandC
    , historyLength :: IORef Int
    }

cardNames :: [String]
cardNames = words "I zero succ dbl get put S K inc dec attack help copy revive zombie"

getCard :: String -> Value
getCard name
  | name == "zero" = VInt 0
  | name `elem` cardNames = VFun name
  | otherwise =
    error $ "card " ++ show name ++ " is invalud"


newState :: IO State
newState = do
  f <- MV.new 256
  v <- MV.new 256
  mon <- MV.new 256
  hist <- MV.new 100000
  histLen <- newIORef 0
  forM_ [0..255] $ \i -> do
    MV.write f i (VFun "I")
    MV.write v i 10000
    MV.write mon i initialMonitor
  return $ State f v mon hist histLen

allDead :: State -> IO Bool
allDead stat = go 0 where
  go !ix
    | ix >= 256 =
      return True
    | otherwise = do
      vi <- MV.read (vital stat) ix
      if vi <= 0
        then go (ix+1)
        else return False

printState :: State -> IO ()
printState stat = do
  go 0
  hPutStrLn stderr "(slots {10000,I} are omitted)"
  where
  go !ix
    | ix >= 256 =
      return ()
    | otherwise = do
        fl <- MV.read (field stat) ix
        vi <- MV.read (vital stat) ix
        case (fl, vi) of
          (VFun "I", 10000) -> do
            return ()
          _ -> do
            hPutStrLn stderr $ show ix ++ "={" ++ show vi ++ "," ++ showValue fl ++ "}"
        go (ix+1)

gcnt :: IORef Int
gcnt = unsafePerformIO $ newIORef 0
{-# NOINLINE gcnt #-}

eval :: Bool -> Value -> State -> State -> IO Value
eval !z !v my opp = do
  cur <- readIORef gcnt
  when (cur >= 1000) $ do
    serror "TLE"
  writeIORef gcnt (cur + 1)
  case v of 
    VApp (VFun "I") a ->
      return a
    VApp (VFun "succ") a ->
      case a of
        VInt n ->
          return $ VInt $ min 65535 $ n + 1
        _ ->
          serror "succ: argument is not integer"
    VApp (VFun "dbl") a ->
      case a of
        VInt n ->
          return $ VInt $ min 65535 $ n * 2
        _ ->
          serror "dbl: argument is not integer"
    VApp (VFun "get") a ->
      case a of
        VInt i -> do
          when (not (i >= 0 && i <= 255)) $ do
            serror "get: invalid slot number"
          hp <- MV.read (vital my) i
          when (hp <= 0) $ do
            serror "get: slot is dead"
          MV.read (field my) i
        _ -> do
          serror "get: argument is not integer"
    VApp (VFun "put") _ ->
      return $ VFun "I"
    VApp (VApp (VApp (VFun "S") f) g) x -> do
      f' <- eval z (VApp f x) my opp
      a' <- eval z (VApp g x) my opp
      eval z (VApp f' a') my opp
    VApp (VApp (VFun "K") x) _ -> 
      return x
    VApp (VFun "inc") a -> do
      case (a, z) of
        (VInt i, False) | i >= 0 && i <= 255 -> do
          vv <- MV.read (vital my) i
          when (vv >= 1 && vv <= 65534) $ do
            MV.write (vital my) i (vv + 1)
        (VInt i, True) | i >= 0 && i <= 255 -> do
          vv <- MV.read (vital my) i
          when (vv >= 1 && vv <= 65535) $ do
            MV.write (vital my) i (vv - 1)
        _ ->
          serror "inc: invalid slot number"
      return $ VFun "I"
    VApp (VFun "dec") a -> do
      case (a, z) of
        (VInt i, False) | i >= 0 && i <= 255 -> do
          vv <- MV.read (vital opp) (255 - i)
          when (vv >= 1 && vv <= 65535) $ do
            MV.write (vital opp) (255 - i) (vv - 1)
        (VInt i, True) | i >= 0 && i <= 255 -> do
          vv <- MV.read (vital opp) (255 - i)
          when (vv >= 1 && vv <= 65534) $ do
            MV.write (vital opp) (255 - i) (vv + 1)
        _ ->
          serror "dec: invalid slot number"
      return $ VFun "I"
    VApp (VApp (VApp (VFun "attack") i) j) n -> do
      case (i, n) of
        (VInt ii, VInt nn) | ii >= 0 && ii <= 255 -> do
          vi <- MV.read (vital my) ii
          when (nn > vi) $ do
            serror "attack: n is greater then vital"
          MV.write (vital my) ii (vi - nn)
          case j of
            VInt jj | jj >= 0 && jj <= 255 -> do
              vj <- MV.read (vital opp) (255 - jj)
              if not z
                then do
                when (vj > 0) $ do
                  MV.write (vital opp) (255 - jj) (max 0 $ vj - (nn * 9 `div` 10))
                else do
                when (vj > 0) $ do
                  MV.write (vital opp) (255 - jj) (min 65535 $ vj + (nn * 9 `div` 10))
            _ -> do
              serror "attack: j is not slot number"
        _ ->
          serror "attack: i is not slot number or n is not number"
      return $ VFun "I"
    VApp (VApp (VApp (VFun "help") i) j) n -> do
      case (i, n) of
        (VInt ii, VInt nn) | ii >= 0 && ii <= 255 -> do
          vi <- MV.read (vital my) ii
          when (nn > vi) $ do
            serror "help: n is greater then vital"
          MV.write (vital my) ii (vi - nn)
          case j of
            VInt jj | jj >= 0 && jj <= 255 -> do
              vj <- MV.read (vital my) jj
              if not z
                then do
                when (vj > 0) $ do
                  MV.write (vital my) jj (min 65535 $ vj + (nn * 11 `div` 10))
                else do
                when (vj > 0) $ do
                  MV.write (vital my) jj (max 0 $ vj - (nn * 11 `div` 10))
            _ -> do
              serror "help: j is not slot number"
        _ ->
          serror "help: i is not slot number or n is not number"
      return $ VFun "I"
    VApp (VFun "copy") i -> do
      case i of
        VInt ii | ii >= 0 && ii <= 255 -> do
          MV.read (field opp) ii
        _ -> do
          serror "copy: argument is not valid slot number"
    VApp (VFun "revive") i -> do
      case i of
        VInt ii | ii >= 0 && ii <= 255 -> do
          vi <- MV.read (vital my) ii
          when (vi == 0) $ do
            MV.write (vital my) ii 1
          return $ VFun "I"
        _ -> do
          serror "revive: argument is not valid slot number"
    VApp (VApp (VFun "zombie") i) x -> do
      case i of
        VInt ii | ii >= 0 && ii <= 255 -> do
          vi <- MV.read (vital opp) (255-ii)
          when (vi > 0) $ do
            serror "zombie: slot is not dead"
          MV.write (field opp) (255-ii) x
          MV.write (vital opp) (255-ii) (-1)
          monAtPos <- MV.read (monitor opp) (255-ii)
          MV.write (monitor opp) (255-ii) $ monAtPos { zombieSeedCount = 1 + zombieSeedCount monAtPos }

          return $ VFun "I"
        _ -> do
          serror "zombie: argument is not valid slot number"
    VApp (VInt _) _ -> do
      serror "Native.Serror"
    _ ->
      return v

serror :: String -> IO a
serror msg = E.throwIO $ LTGError msg

{-# Language BangPatterns #-}

module Simulator (
  Simulator(..),
  newSimulator,
  execStep,
  
  myState,
  oppState,
  printState,
  
  Hand,
  ) where

import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import qualified Data.Vector.Mutable as MV
import System.IO.Unsafe

data Simulator
  = Simulator
    { turnCnt :: !Int
    , phase   :: !Bool
    , p1State :: State
    , p2State :: State
    }

newSimulator :: IO Simulator
newSimulator = do
  s1 <- newState
  s2 <- newState
  return $ Simulator 0 True s1 s2

type Hand = (Int, Int, String)

myState :: Simulator -> State
myState Simulator { phase = True, p1State = stat } = stat
myState Simulator { p2State = stat } = stat

oppState :: Simulator -> State
oppState Simulator { phase = True, p2State = stat } = stat
oppState Simulator { p1State = stat } = stat

execStep :: Hand -> Simulator -> IO (Simulator, String)
execStep (typ, pos, name) s = do
  em <- allDead my
  eo <- allDead opp
  
  case (em, eo) of
    (True, True) -> do
      return (s, "!! draw")
    (True, _) -> do
      return (s, "!! player" ++ show (if phase s then 1 else 0) ++ " win")
    (_, True) -> do
      return (s, "!! player" ++ show (if phase s then 0 else 1) ++ " win")
    _ | turnCnt s >= 10000 -> do
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
              print e
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
            print e
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

data State
  = State
    { field :: MV.IOVector Value
    , vital :: MV.IOVector Int
    }

cards :: [String]
cards = words "I zero succ dbl get put S K inc dec attack help copy revive zombie"

getCard :: String -> Value
getCard name
  | name == "zero" = VInt 0
  | name `elem` cards = VFun name
  | otherwise =
    error $ "card " ++ show name ++ " is invalud"

newState :: IO State
newState = do
  f <- MV.new 256
  v <- MV.new 256
  forM_ [0..255] $ \i -> do
    MV.write f i (VFun "I")
    MV.write v i 10000
  return $ State f v

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
  putStrLn "(slots {10000,I} are omitted)"
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
            putStrLn $ show ix ++ "={" ++ show vi ++ "," ++ showValue fl ++ "}"
        go (ix+1)

gcnt :: IORef Int
gcnt = unsafePerformIO $ newIORef 0
{-# NOINLINE gcnt #-}

eval :: Bool -> Value -> State -> State -> IO Value
eval !z !v my opp = do
  cur <- readIORef gcnt
  when (cur >= 1000) $ do
    error "TLE"
  writeIORef gcnt (cur + 1)
  case v of 
    VApp (VFun "I") a ->
      return a
    VApp (VFun "succ") a ->
      case a of
        VInt n ->
          return $ VInt $ min 65535 $ n + 1
        _ ->
          error "succ: argument is not integer"
    VApp (VFun "dbl") a ->
      case a of
        VInt n ->
          return $ VInt $ min 65535 $ n * 2
        _ ->
          error "dbl: argument is not integer"
    VApp (VFun "get") a ->
      case a of
        VInt i -> do
          when (not (i >= 0 && i <= 255)) $ do
            error "get: invalid slot number"
          hp <- MV.read (vital my) i
          when (hp <= 0) $ do
            error "get: slot is dead"
          MV.read (field my) i
        _ -> do
          error "get: argument is not integer"
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
          error "inc: invalid slot number"
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
          error "dec: invalid slot number"
      return $ VFun "I"
    VApp (VApp (VApp (VFun "attack") i) j) n -> do
      case (i, n) of
        (VInt ii, VInt nn) | ii >= 0 && ii <= 255 -> do
          vi <- MV.read (vital my) ii
          when (nn > vi) $ do
            error "attack: n is greater then vital"
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
              error "attack: j is not slot number"
        _ ->
          error "attack: i is not slot number or n is not number"
      return $ VFun "I"
    VApp (VApp (VApp (VFun "help") i) j) n -> do
      case (i, n) of
        (VInt ii, VInt nn) | ii >= 0 && ii <= 255 -> do
          vi <- MV.read (vital my) ii
          when (nn > vi) $ do
            error "help: n is greater then vital"
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
              error "help: j is not slot number"
        _ ->
          error "help: i is not slot number or n is not number"
      return $ VFun "I"
    VApp (VFun "copy") i -> do
      case i of
        VInt ii | ii >= 0 && ii <= 255 -> do
          MV.read (field opp) ii
        _ -> do
          error "copy: argument is not valid slot number"
    VApp (VFun "revive") i -> do
      case i of
        VInt ii | ii >= 0 && ii <= 255 -> do
          vi <- MV.read (vital my) ii
          when (vi == 0) $ do
            MV.write (vital my) ii 1
          return $ VFun "I"
        _ -> do
          error "revive: argument is not valid slot number"
    VApp (VApp (VFun "zombie") i) x -> do
      case i of
        VInt ii | ii >= 0 && ii <= 255 -> do
          vi <- MV.read (vital opp) (255-ii)
          when (vi > 0) $ do
            error "zombie: slot is not dead"
          MV.write (field opp) (255-ii) x
          MV.write (vital opp) (255-ii) (-1)
          return $ VFun "I"
        _ -> do
          error "zombie: argument is not valid slot number"
    VApp (VInt _) _ -> do
      error "Native.Error"
    _ ->
      return v
  

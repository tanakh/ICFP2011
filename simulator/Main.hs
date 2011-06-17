{-# Language BangPatterns #-}

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import qualified Data.Vector.Mutable as MV
import System.Environment
import System.Exit
import System.IO
import System.Process

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
printState stat = go 0 where
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

eval :: Int -> Value -> State -> State -> IO Value
eval !cnt !v my opp
  | cnt >= 1000 = error "TLE"
  | otherwise = case v of 
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
        return $ VInt $ min 65535 $ n * n
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
  VApp (VApp (VFun "put") _) y ->
    return y
  VApp (VApp (VApp (VFun "S") f) g) x ->
    eval (cnt+1) (VApp (VApp f x) (VApp g x)) my opp
  VApp (VApp (VFun "K") x) _ -> 
    return x
  VApp (VFun "inc") a -> do
    case a of
      VInt i | i >= 0 && i <= 255 -> do
        vv <- MV.read (vital my) i
        when (vv >= 1 && vv <= 65534) $ do
          MV.write (vital my) i (vv + 1)
      _ ->
        error "inc: invalid slot number"
    return $ VFun "I"
  VApp (VFun "dec") a -> do
    case a of
      VInt i | i >= 0 && i <= 255 -> do
        vv <- MV.read (vital opp) (255 - i)
        when (vv >= 1 && vv <= 65535) $ do
          MV.write (vital opp) (255 - i) (vv - 1)
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
            MV.write (vital opp) (255 - jj) (max 0 $ vj - (nn * 9 `div` 10))
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
            MV.write (vital my) jj (min 65535 $ vj + (nn * 11 `div` 10))
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
        vi <- MV.read (vital opp) ii
        when (vi > 0) $ do
          error "zombie: slot is not dead"
        MV.write (field opp) ii x
        MV.write (vital opp) ii (-1)
        return $ VFun "I"
      _ -> do
        error "zombie: argument is not valid slot number"
  VApp (VInt _) _ -> do
    error "Native.Error"
  _ ->
    return v
    
type Proc = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

input :: State -> Proc -> Proc -> IO (Value, Int)
input my (_, Just inp, _, _) (Just out, _, _, _) = do
  putStrLn "(1) apply card to slot, or (2) apply slot to card?"
  typ <- read <$> hGetLine inp
  hPrint out $ show typ
  hFlush out
  case typ of
    1 -> do
      putStrLn "card name?"
      cname <- hGetLine inp
      hPutStrLn out cname
      hFlush out
      putStrLn "slot no?"
      pos <- read <$> hGetLine inp
      hPutStrLn out $ show pos
      hFlush out
      val <- MV.read (field my) pos
      return (VApp (getCard cname) val, pos)
    2 -> do
      putStrLn "slot no?"
      pos <- read <$> hGetLine inp
      hPutStrLn out $ show pos
      hFlush out
      putStrLn "card name?"
      cname <- hGetLine inp
      hPutStrLn out cname
      hFlush out
      val <- MV.read (field my) pos
      return (VApp val (getCard cname), pos)

play :: Int -> Int -> State -> State -> Proc -> Proc -> IO ()
play !turn !pid my opp p1 p2 = do
  md <- allDead my
  od <- allDead opp
  
  when (md && od) $ do
    putStrLn "!! draw"
    exitSuccess
  
  when md $ do
    putStrLn $ "!! player " ++ (show $ 1-pid) ++ " win"
    exitSuccess

  when od $ do
    putStrLn $ "!! player " ++ (show $ pid) ++ " win"
    exitSuccess
  
  when (turn >= 200000) $ do
    putStrLn "!! draw"
    exitSuccess
  
  when (turn `mod` 2 == 0) $ do
    putStrLn $ "###### turn " ++ (show $ (turn `div` 2) + 1)
  
  putStrLn $ "*** player " ++ (show pid) ++ "'s turn, with slots:"
  
  printState my
  
  putStrLn "(slots {10000,I} are omitted)"

  (val, pos) <- input my p1 p2
  eres <- E.try $ eval 0 val my opp
  case eres of
    Left (E.SomeException e) -> do
      print e
      MV.write (field my) pos (VFun "I")
    Right res ->
      MV.write (field my) pos res
  play (turn+1) (1-pid) opp my p2 p1

main :: IO ()
main = do
  ["match", prog1, prog2] <- getArgs
  
  proc1 <- createProcess (shell $ prog1 ++ " 0") { std_in = CreatePipe, std_out = CreatePipe}
  proc2 <- createProcess (shell $ prog1 ++ " 1") { std_in = CreatePipe, std_out = CreatePipe}

  a <- newState
  b <- newState
  
  play 0 0 a b proc1 proc2
  
  return ()

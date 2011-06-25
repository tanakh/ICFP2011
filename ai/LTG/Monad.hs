{-# Language BangPatterns #-}
module LTG.Monad (
  LTG,
  runLTG,
  runLTGs,
  
  right, ($<),
  left, ($>),
  
  findAlive,
  isAlive,
  isDead,
  getTurnCnt, 
  getPhase,
  getVital,
  getField,
  getMonitor,
  getHistory,
  getHistoryLength,
  getHistoryReverse,
  getState,
  getSimulator, 

  nop, 
  lerror, lprint,
  ) where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception.Control as E
import Control.Monad
import Control.Monad.State hiding (State)
import Data.List
import Data.IORef (readIORef)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MV
import System.Environment
import System.Exit
import System.IO

import LTG.Base
import LTG.Exception
import LTG.Simulator

runLTG :: LTG a -> IO ()
runLTG ltg = do
  (teban:_) <- reverse <$> getArgs
  
  let ltg' = f teban
  
  siml <- newSimulator 
  flip evalStateT (SingleThread,siml) $ do
    er <- E.try ltg'
    case er of
      Left (LTGError msg) -> do
        lprint msg
        forever nop
      Right r -> do
        return ()
  where
    f "0" = ltg
    f "1" = getHand >> ltg

type LTG = StateT (ThreadAttr, Simulator) IO

infix 1 $<
infix 1 $>

--------------------------------------------- LTG coroutine
data ThreadAttr = SingleThread | 
                MultiThread (TVar Scheduler) Int{-tid-} 

data Scheduler 
  = Scheduler 
    { mailbox   :: Vector(TMVar Simulator),
      condition :: Vector(LTG Bool)
    }


atomicalift :: STM a -> LTG a
atomicalift = liftIO . atomically

masterTID::Int
masterTID = 0

yield :: LTG ()
yield = do
  th <- getThreadAttr
  case th of
    SingleThread -> return ()
    MultiThread tvs _ -> do
             sim <- getSimulator
             schdr <- atomicalift $ readTVar tvs
             atomicalift $ putTMVar (mailbox schdr ! masterTID) sim
             waitWake
             
waitWake :: LTG ()
waitWake = do
  th <- getThreadAttr
  case th of
    SingleThread -> lerror "single thread program must not sleep; no one else will wake her"
    MultiThread tvs tid -> do
             schdr <- atomicalift $ readTVar tvs
             sim <- atomicalift $ takeTMVar (mailbox schdr ! tid) 
             putSimulator sim

masterThread :: LTG ()
masterThread = forever $ do
  th <- getThreadAttr
  case th of
    SingleThread -> lerror "no need for master thread in SingleThread mode"
    MultiThread tvs _ -> do
                 schdr <- atomicalift $ readTVar tvs
                 turn  <- getTurnCnt
                 conds <- Vector.mapM id (condition schdr)
                 turn' <- getTurnCnt
                 when (turn /= turn') $ lerror "conditions must not consume turn!"
                 let f = Vector.findIndex id conds
                 case f of
                   Nothing  -> lerror "error in condition"
                   Just tid -> do
                       sim <- getSimulator
                       atomicalift $ putTMVar (mailbox schdr ! tid) sim
                       waitWake
                       

runLTGs :: [(LTG Bool, LTG ())] -> IO ()
runLTGs ltgs0 = do
  (teban:_) <- reverse <$> getArgs

  let ltgs = ltgs0 ++ [(return True, tailWorker)]
      n = length ltgs


  mailbox0  <- newSimulator >>= newTMVarIO
  mailboxes <- replicateM n newEmptyTMVarIO 
  let ltgPrefix "0" = return ()
      ltgPrefix "1" = getHand 
      
      schdr = Scheduler 
              { mailbox   = Vector.fromList $ mailbox0 : mailboxes
              , condition = Vector.fromList $ return False : map fst ltgs
              }
  tvs <- newTVarIO schdr

  forM (zip [1..] $ map snd ltgs) (\(tid, ltg) -> forkIO $ runLTGThread tvs tid ltg)
  runLTGThread tvs masterTID (ltgPrefix teban >> masterThread)


runLTGThread :: TVar Scheduler -> Int -> LTG a -> IO ()
runLTGThread tvs tid ltg = do
  flip evalStateT (MultiThread tvs tid , error "undefined state") $ do
                     waitWake
                     er <- E.try ltg
                     case er of
                       Left (LTGError msg) -> do
                               lprint msg
                               unregister 
                               forever nop
                       Right r -> do
                               unregister 
                               forever nop

unregister :: LTG ()
unregister = do
  th <- getThreadAttr
  case th of
    SingleThread -> lerror "no way to unregister"
    MultiThread tvs tid -> do
               schdr <- atomicalift $ readTVar tvs
               let newSchdr = schdr{condition = condition schdr // [(tid, return False)]}
               atomicalift $ writeTVar tvs newSchdr

tailWorker :: LTG ()
tailWorker = forever $ do
               lprint "defaulting to tailWorker"
               nop

--------------------------------------------- end LTG coroutine
putHand :: Hand -> LTG ()
putHand h @ (t, s, c) = do
  liftIO $ do
    print t
    if t == 1
      then do
      putStrLn c
      print s
      else do
      print s
      putStrLn c
    hFlush stdout
  
  s <- getSimulator
  (ns, msg) <- liftIO $ execStep h s
  unless (null msg) $ do
    liftIO $ exitSuccess
  putSimulator ns


readHand :: IO Hand
readHand = do
  t <- readLn
  if t == 1
    then do
    c <- getLine
    s <- readLn
    return (t, s, c)
    else do
    s <- readLn
    c <- getLine
    return (t, s, c)
    
getHand :: LTG ()
getHand = do
  h <- liftIO readHand
  
  s <- getSimulator
  (ns, msg) <- liftIO $ execStep h s
  putSimulator ns
  
  unless (null msg) $ do
    liftIO $ exitSuccess
  yield

right, ($<) :: Int -> Card -> LTG ()
right s c = do
  v <- getVital True s
  when (v <= 0) $ lerror $ "dead card apply "++ show s
  putHand (2, s, cardName c)
  getHand

($<) = right

left, ($>) :: Card -> Int -> LTG ()
left c s = do
  v <- getVital True s
  when (v <= 0) $ lerror $ "dead card apply " ++ show s
  putHand (1, s, cardName c)
  getHand

($>) = left

findAlive :: Bool -> (Int -> Bool) -> LTG Int
findAlive my pred = go 0 where
  go !ix = do
    if pred ix
      then do      
      b <- isAlive my ix
      if b
        then return ix
        else go (ix+1)
      else go (ix+1)

isAlive :: Bool -> Int -> LTG Bool
isAlive my ix = do
  v <- getVital my ix
  return $ v > 0

isDead :: Bool -> Int -> LTG Bool
isDead my ix = not <$> isAlive my ix


getTurnCnt :: LTG Int
getTurnCnt = turnCnt <$> getSimulator

getPhase :: LTG Bool
getPhase = phase <$> getSimulator

getVital :: Bool -> Int -> LTG Int
getVital my ix = do
  st <- getState my
  liftIO $ MV.read (vital st) ix

getField :: Bool -> Int -> LTG Value
getField my ix = do
  st <- getState my
  liftIO $ MV.read (field st) ix

getMonitor :: Bool -> Int -> LTG Monitor
getMonitor my ix = do
  st <- getState my
  liftIO $ MV.read (monitor st) ix

getHistory :: Bool -> Int -> LTG HandC
getHistory my t = do
  st <- getState my
  liftIO $ MV.read (history st) t

getHistoryReverse :: Bool -> Int -> LTG HandC
getHistoryReverse my t = do
  len <- getHistoryLength my
  st <- getState my
  liftIO $ MV.read (history st) (len-t-1)


getHistoryLength :: Bool -> LTG Int
getHistoryLength my = do
  st <- getState my
  liftIO $ readIORef (historyLength st) 

getState :: Bool -> LTG State
getState my = do
  s <- getSimulator
  return $ if my then myState s else oppState s

getSimulator :: LTG Simulator
getSimulator = snd <$> get

getThreadAttr :: LTG ThreadAttr
getThreadAttr = fst <$> get


putSimulator :: Simulator -> LTG ()
putSimulator s = do
  (sch, _) <- get
  put (sch, s)


nop :: LTG ()
nop = do
  ix <- findAlive True (const True)
  I $> ix

lerror :: String -> LTG ()
lerror msg = E.throwIO $ LTGError msg

lprint :: Show a => a -> LTG ()
lprint v = liftIO $ hPrint stderr v

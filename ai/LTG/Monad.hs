{-# Language BangPatterns #-}
module LTG.Monad (
  LTG,
  runLTG,
  
  right, ($<),
  left, ($>),
  
  findAlive,
  isAlive,
  getVital,
  getVitalCurve,
  getField,
  getMonitor,
  getHistory,
  getHistoryLength,
  getHistoryReverse,
  getState,
  
  nop, 
  lerror,
  lprint,
  ) where

import Control.Applicative
import qualified Control.Exception.Control as E
import Control.Monad
import Control.Monad.State hiding (State)
import Data.IORef (readIORef)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
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
  flip evalStateT siml $ do
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

type LTG = StateT Simulator IO

infix 1 $<
infix 1 $>

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
  
  s <- get
  (ns, msg) <- liftIO $ execStep h s
  unless (null msg) $ do
    liftIO $ exitSuccess
  put ns

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
  
  s <- get
  (ns, msg) <- liftIO $ execStep h s
  put ns
  
  unless (null msg) $ do
    liftIO $ exitSuccess

right, ($<) :: Int -> Card -> LTG ()
right s c = do
  v <- getVital True s
  when (v <= 0) $ lerror "dead card apply"
  putHand (2, s, cardName c)
  getHand

($<) = right

left, ($>) :: Card -> Int -> LTG ()
left c s = do
  v <- getVital True s
  when (v <= 0) $ lerror "dead card apply"
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

getVital :: Bool -> Int -> LTG Int
getVital my ix = do
  st <- getState my
  liftIO $ MV.read (vital st) ix

getVitalCurve :: Bool -> LTG (Vector Int)
getVitalCurve my = do
  vitals <- mapM (getVital my) [0..255]
  return $ V.fromList vitals 

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
  s <- get
  return $ if my then myState s else oppState s

nop :: LTG ()
nop = do
  ix <- findAlive True (const True)
  I $> ix

lerror :: String -> LTG ()
lerror msg = E.throwIO $ LTGError msg

lprint :: Show a => a -> LTG ()
lprint v = liftIO $ hPrint stderr v

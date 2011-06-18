{-# Language BangPatterns #-}
module LTG.Monad (
  LTG,
  runLTG,
  
  right, ($<),
  left, ($>),
  
  findAlive,
  isAlive,
  getVital,
  getField,
  getState,
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (State)
import qualified Data.Vector.Mutable as MV
import System.Environment
import System.Exit
import System.IO

import LTG.Base
import LTG.Simulator

runLTG :: LTG a -> IO a
runLTG ltg = do
  (teban:_) <- reverse <$> getArgs
  
  let ltg' = f teban
  
  siml <- newSimulator
  evalStateT ltg' siml
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
  putHand (2, s, cardName c)
  getHand

($<) = right

left, ($>) :: Card -> Int -> LTG ()
left c s = do
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

getField :: Bool -> Int -> LTG Value
getField my ix = do
  st <- getState my
  liftIO $ MV.read (field st) ix

getState :: Bool -> LTG State
getState my = do
  s <- get
  return $ if my then myState s else oppState s

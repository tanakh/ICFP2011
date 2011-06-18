module LTG.Monad (
  LTG,
  runLTG,
  
  right, ($<),
  left, ($>),
  ) where

import Control.Monad
import Control.Monad.State
import Data.Text as T
import Data.Text.IO as T
import System.Io

import LTG.Simulator

runLTG :: String -> LTG a -> IO a
runLTG teban ltg = do
  siml <- newSimulator
  evalState ltg' siml
  where
    ltg' | teban == "0" = ltg
         | teban == "1" = getHand >> ltg

type LTG = StateT Simulator IO

infix 1 $<
infix 1 $>

type Hand = (Int, Int , String)

putHand :: Hand -> LTG ()
putHand h @ (t, s, c) = do
  print t
  if t == 1
    then do
    ptint c
    ptint s
    else do
    print s
    print c
  hFlush stdout
  
  s <- get
  (ns, msg) <- liftIO $ execStep h s
  unless (null msg) $ do
    exitSuccess
  put ns

readHand :: LTG Hand
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
  h <- readHand
  
  s <- get
  ns <- liftIO $ execStep h s
  put ns

right, ($<) :: Int -> Card -> LTG ()
right s c = do
  putHand (2, s, cardName c)
  getHand

($<) = right

left, ($>) :: Card -> Int -> LTG ()
left c s = do
  putHand (1, s, c)
  getHand

($>) = left

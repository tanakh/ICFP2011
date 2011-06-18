module LTG.Monad (
  LTG,
  runLTG,
  
  right, ($<),
  left, ($>),
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
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

#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Monad
import System.Environment
import System.Exit
import System.IO
import System.Process

import Simulator

type Proc = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

input :: Proc -> Proc -> IO Hand
input (_, Just inp, _, _) (Just out, _, _, _) = do
  putStrLn "(1) apply card to slot, or (2) apply slot to card?"
  typ <- read <$> hGetLine inp
  hPrint out typ
  hFlush out
  case typ of
    1 -> do
      putStrLn "card name?"
      cname <- hGetLine inp
      hPutStrLn out cname
      hFlush out
      putStrLn "slot no?"
      pos <- read <$> hGetLine inp
      hPrint out pos
      hFlush out
      return (typ, pos, cname)
    2 -> do
      putStrLn "slot no?"
      pos <- read <$> hGetLine inp
      hPrint out pos
      hFlush out
      putStrLn "card name?"
      cname <- hGetLine inp
      hPutStrLn out cname
      hFlush out
      return (typ, pos, cname)

play :: Proc -> Proc -> Simulator -> IO ()
play p1 p2 s = do
  when (phase s) $ do
    putStrLn $ "###### turn " ++ (show $ turnCnt s + 1)
  
  putStrLn $ "*** player " ++ (show $ if phase s then 0 else 1) ++ "'s turn, with slots:"
  printState $ myState s

  hand <- input p1 p2
  print hand
  (ns, msg) <- execStep hand s
  
  if msg /= ""
    then
    putStrLn msg
    else
    play p2 p1 ns

main :: IO ()
main = do
  ["match", prog1, prog2] <- getArgs
  
  proc1 <- createProcess (shell $ prog1 ++ " 0") { std_in = CreatePipe, std_out = CreatePipe}
  proc2 <- createProcess (shell $ prog2 ++ " 1") { std_in = CreatePipe, std_out = CreatePipe}

  s <- newSimulator
  play proc1 proc2 s
  
  return ()

{-# OPTIONS -Wall #-}
import Control.Monad
import LTG
import System.Environment

skip :: IO ()
skip = do
  _ <- getLine
  _ <- getLine
  _ <- getLine
  return ()


main :: IO ()  
main = do
  side <- fmap (read . head) getArgs 
  when (side == (1::Int)) skip
  play


play :: IO ()  
play = do
  I $> 0
  skip
  play
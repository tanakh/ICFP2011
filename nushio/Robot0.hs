{-# OPTIONS -Wall #-}
import Control.Monad
import LTG hiding (($<) , ($>))
import System.Environment

skip :: IO ()
skip = do
  _ <- getLine
  _ <- getLine
  _ <- getLine
  return ()

infix 3 $<
infix 3 $>

($<) :: Int -> Card -> IO ()
($<) x y = do
  right x y
  skip

($>) :: Card -> Int -> IO ()  
($>) x y = do
  left x y
  skip


main :: IO ()  
main = do
  side <- fmap (read . head) getArgs 
  when (side == (1::Int)) skip
  S $> 255
  S $> 255
  S $> 255
  play


play :: IO ()  
play = do
  1 $< Zero 
  Dec $> 1
  play


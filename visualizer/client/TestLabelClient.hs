{-# OPTIONS -Wall #-}
import System.Posix.Unistd

main :: IO ()
main = do
  sub 1
  
sub :: Integer -> IO ()  
sub x = do
  usleep 100000
  putStrLn $ "label 100 100 1 " ++ show x
  sub $ (3*x+1) `mod` 2^(40::Int)
  

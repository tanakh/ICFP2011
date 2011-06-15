{-# OPTIONS -Wall #-}
import System.IO
import System.Posix.Unistd

main :: IO ()
main = do
  sub 0 1
  
sub :: Double -> Integer -> IO ()  
sub t n = do
  let t' = t + 0.01
      n' = n+1
      draw x y n'' = putStrLn $ unwords ["label", show x, show y, "1", show n'']
  draw (0.5 * (1 + cos t)) (0.5 * (1 + sin t)) n
  draw (0.5 * (1 + cos (2*t))) (0.5 * (1 + sin (pi * t))) n
  draw (0.5 * (1 + cos t)) (0.5 * (1 - sin t)) (3*n)
  putStrLn "flush"
  hFlush stdout
  usleep 16000
  sub t' n'
  

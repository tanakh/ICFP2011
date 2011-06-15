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
      draw fn x y n'' = putStrLn $ unwords 
        ["draw", show x, show y, "1", fn, "2", show n'']
--        ["label", show x, show y, "1", "pattern.png", "1", show n'']
  draw "0-0.png" (0.5 * (1 + cos (sqrt 3 * t))) (0.5 * (1 + sin t)) 96
  draw "1-1.png" (0.5 * (1 + cos t)) (0.5 * (1 + sin t)) n
  draw "2-1.png" (0.5 * (1 + cos (2*t))) (0.5 * (1 + sin (pi * t))) n
  draw "4-1.png" (0.5 * (1 + cos t)) (0.5 * (1 - sin t)) (3*n)
  putStrLn "flush"
  hFlush stdout
  usleep 16000
  sub t' n'
  

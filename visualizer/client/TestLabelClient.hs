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
      draw fn x y text = putStrLn $ unwords 
        ["draw", show x, show y, "1", fn, "1", text]
--        ["label", show x, show y, "1", "pattern.png", "1", show n'']
  draw "0-0.png" (0.5 * (1 + cos (sqrt 3 * t))) (0.5 * (1 + sin t)) 
           ("boku to keiyaku!"++show n)
  draw "1-1.png" (0.5 * (1 + cos t)) (0.5 * (1 + sin t)) 
           "uze- cho-uze-!"
  draw "2-1.png" (0.5 * (1 + cos (2*t))) (0.5 * (1 + sin (pi * t))) 
           "mamorimakucchaimaskarane--"
  draw "4-1.png" (0.5 * (1 + cos t)) (0.5 * (1 - sin t))
           "soreni wa oyobanaiwa!"
  putStrLn "flush"
  hFlush stdout
  usleep 16000
  sub t' n'
  

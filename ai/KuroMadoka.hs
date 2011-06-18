{-# OPTIONS -Wall #-}
import LTG 
import LTG.SoulGems

sittingDuck :: LTG()
sittingDuck = do
  I $> 0
  sittingDuck

loop2S :: Int -> LTG()
loop2S i = do
  147 $< S
  if i > 0
    then loop2S (i-1)
    else return ()

loop1S :: Int -> LTG()
loop1S i = do
  S $> 147
  if i > 0
    then loop1S (i-1)
    else return ()

main :: IO ()
main = runLTG $ do
  loop2S 64
  S $> 147
  S $> 147
  loop2S 500
  S $> 147
  S $> 147
  loop1S 50000
  loop2S 50000
  sittingDuck


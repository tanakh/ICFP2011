import Control.Applicative
import Control.Monad
import System.IO

import LTG
import LTG.SoulGems

isDead :: Bool -> Int -> LTG Bool
isDead my ix = not <$> isAlive my ix

main :: IO ()
main = runLTG $ do
  forever $ do
    ds <- filterM (isDead True) [0..255]
    if null ds
      then nop
      else do
      revive (head ds)
      return ()

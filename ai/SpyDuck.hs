#!/usr/bin/env runhaskell
import LTG

import Control.Applicative
import Control.Monad
import Control.Monad.State

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  forever $ do
    nop
    spy "enemy slot use"      False propHandCount   (>0)
    spy "my zombie weakpoint" True  zombieSeedCount (>0)

spy :: (Show a) => String -> Bool -> (Monitor -> a) -> (a -> Bool) ->LTG ()
spy label my dataRecord isImportant = do
  mons <- forM [0..255] $ getMonitor my
  let addrMons = zip [0..255] mons
      info = filter ( isImportant . dataRecord . snd ) addrMons
      prettyInfo = map (\(i,m) -> (i, dataRecord m)) info
  t <- turnCnt <$> get
  lprint $ "turn" ++ show t ++ " : (addr, " ++ label ++ ") = " ++ show prettyInfo


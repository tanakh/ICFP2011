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
    I $> 0
    mons <- forM [0..255] $ getMonitor False 
    let addrMons = zip [0..255] mons
        info = filter ( (>0) . propHandCount . snd ) addrMons
        prettyInfo = map (\(i,m) -> (i, propHandCount m)) info
    t <- turnCnt <$> get
    lprint $ "turn" ++ show t ++ " : (addr, access freq.) = " ++ show prettyInfo

#!/usr/bin/env runhaskell
import LTG

import Control.Applicative
import Control.Monad

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  forever $ do
    spy "enemy slot use"      False propHandCount   (>0)
    spy "my zombie weakpoint" True  zombieSeedCount (>0)
    howEnemyMakeZombie
    nop

spy :: (Show a) => String -> Bool -> (Monitor -> a) -> (a -> Bool) ->LTG ()
spy label my dataRecord isImportant = do
  mons <- forM [0..255] $ getMonitor my
  let addrMons = zip [0..255] mons
      info = filter ( isImportant . dataRecord . snd ) addrMons
      prettyInfo = map (\(i,m) -> (i, dataRecord m)) info
  t <- getTurnCnt
  lprint $ "turn" ++ show t ++ " : (addr, " ++ label ++ ") = " ++ show prettyInfo


howEnemyMakeZombie :: LTG ()
howEnemyMakeZombie = do
  grepHistory False Zombie
  len <- getHistoryLength False 
  lprint $ "enemy's history len : " ++ show len
  h <- getHistoryReverse False 0 
  lprint $ "enemy's last hand :" ++ show h
  h <- getHistory False 0 
  lprint $ "enemy's first hand :" ++ show h
  

grepHistory :: Bool -> Card -> LTG ()
grepHistory my card = do
  n <- getHistoryLength my
  hist <- mapM (getHistory my) [0..n-1] 
  let tHist = zip [0..n-1] hist
      ftHist = filter ((==card) . get33 . snd) tHist
  lprint $ "enemy 's use of " ++ show card ++ " " ++ show ftHist

{-
  bl <- getBacklog False
  let t = length bl
      timelog = zip [t-1, t-2 ..] bl
      tlZombie = filter ((==Zombie) . get33 . snd ) timelog
      tlHelp = filter ((==Help) . get33 . snd ) timelog
  lprint $ "enemy's use of Zombie cards (turn, hand): " ++ show tlZombie
  lprint $ "enemy's use of Help cards (turn, hand): " ++ show tlHelp
  lprint $ "enemy's recent moves: " ++ show (take 5 timelog)
          
      -}
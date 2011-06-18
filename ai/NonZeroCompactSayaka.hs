{-# OPTIONS -Wall #-}
import LTG 
import LTG.SoulGems




-- inject zombie that attack
inject_sayasayaFA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> LTG ()
inject_sayasayaFA i1 i2 ffa f1 f2 fdmg fgain dmg = do
  -- v[f1] <- S (K (attack fdmg fgain))
  num i1 fdmg
  Attack $> i1
  num i2 (255 - fgain)
  applyFA i1 i2 ffa f1 i1 i2
  K $> f1
  S $> f1
  -- v[i2] <- K dmg
  num i2 dmg
  K $> i2
  -- Sayasaya ready
  applyFA i1 i2 ffa f1 f1 i2
  -- Inject!
  clear f2
  f2 $< Zero
  Zombie $> f2
  applyFA i1 i2 ffa f1 f2 f1


sittingDuck :: LTG()
sittingDuck = do
  I $> 0
  sittingDuck

attackLoopFA :: Int -> Int -> Int -> Int -> Int -> Int -> LTG()
attackLoopFA i1 i2 ffa v k s = do
  attackFA          i1 i2 ffa 3 v k 8192
  attackFA          i1 i2 ffa 3 (v+1) k 8192
  inject_sayasayaFA i1 i2 ffa 3 4 s s 10000
  if v > 240 
    then do
      attackLoopFA i1 i2 ffa 5 (k+1) 0
    else attackLoopFA i1 i2 ffa (v+2) (k+1) (s+1)

main :: IO ()
main = runLTG $ do
  futureApply 1 2 18 3
  attack      0 0 10000
--  attackFA    1 2 18 3 5 6 8192
  attackLoopFA 1 2 18 5 0 0
  sittingDuck




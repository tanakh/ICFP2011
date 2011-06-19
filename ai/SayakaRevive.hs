#!/usr/bin/env runhaskell
import Control.Applicative
import qualified Control.Exception.Control as E
import Control.Monad
import Data.List

import LTG
import LTG.SoulGems

getVitals :: Bool -> LTG [Int]
getVitals my = do
  forM [0..255] $ \i -> do
    getVital my i

maximize :: Int -> LTG ()
maximize ix = do
  hp <- getVital True ix
  when (hp < 65535) $ do
    lprint $ ("%%%%%", hp, ix)
    num 0 ix
    clear 1
    1 $< Help
    apply0 1
    apply0 1
    num 0 (hp-1)
    apply0 1
    maximize ix

kill :: Int -> LTG ()
kill target = do
  vs <- getVitals True
  let (_, ix) = head $ reverse $ sort $ zip vs [(0::Int)..]
  ix <- return 2
  maximize ix
  attack ix (255-target) 11112

killAny :: LTG ()
killAny = do
  ix <- findAlive False (const True)
  lprint ("&&&&&", ix)
  kill ix

main :: IO ()
main = runLTG $ do
  forever $ do
    ds <- filterM (isDead True) [0..255]
    if null ds
      then do
      mb <- E.try killAny
      case mb of
        Left (LTGError e) -> do
          lprint e
          return ()
        Right _ -> do
          return ()
      return ()
      else do
      revive (head ds)
      return ()

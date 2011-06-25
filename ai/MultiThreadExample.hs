#!/usr/bin/env runhaskell
import LTG

import Control.Applicative
import Control.Monad


i1, i2, i3 :: Int
(i1: i2: i3: i4 : _) = [1..]

main :: IO ()
main = runLTGs
       [((>10) <$> getVital False 0, shootAtk ),
        (isAlive False 0           , shootDec ),
        (return True               , shootInc )]

shootAtk :: LTG ()
shootAtk = forever $ do
             (v, _, i) <- maximum <$> mapM makeVii [1..255]
             num i1 i
             Attack $> i1
             num 0 255
             apply0 i1
             replicateM_ 5 $ Dbl $> 0
             getField True 0 >>= lprint
             getField True i1 >>= lprint
             apply0 i1

shootDec :: LTG ()
shootDec = forever $ do
             num i2 255
             copyTo i3 i2
             lprint "Dec"
             Dec $> i3
             
shootInc :: LTG ()
shootInc = forever $ do
             num i4 0
             lprint "Inc"
             Inc $> i4


makeVii :: Int -> LTG (Int, Int, Int)                     
makeVii i = do
  v <- getVital True i
  return (v,-i, i)
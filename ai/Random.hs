import LTG

import Control.Monad
import Control.Monad.State
import Data.Vector ((!))
import qualified Data.Vector as V
import System.Random

main :: IO ()
main = do 
  setStdGen $ mkStdGen 0x9b
  sr <- randomRIO (0,8::Int)
  runLTG $ ltgMain (2^sr)

ltgMain :: Int -> LTG ()
ltgMain slotRange = do
  forever $ do
    s  <- liftIO $ randomRIO (0, slotRange-1)
    ci  <- liftIO $ randomRIO (0, V.length cards - 1)
    lr <- liftIO $ randomRIO (0, 1::Int)
    let c = cards ! ci
    if lr == 0
    then s $< c
    else c $> s
    

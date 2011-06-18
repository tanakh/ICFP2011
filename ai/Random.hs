import LTG

import Control.Monad
import Control.Monad.State
import Data.Vector ((!))
import qualified Data.Vector as V
import System.Random

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  forever $ do
    c <- liftIO $ randomRIO (0, V.length cards - 1)
    I $> 0

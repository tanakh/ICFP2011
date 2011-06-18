import LTG

import Control.Monad
import Control.Monad.State

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  forever $ do
    I $> 0
    s <- get
    liftIO $ printState $ myState s
    liftIO $ printState $ oppState s

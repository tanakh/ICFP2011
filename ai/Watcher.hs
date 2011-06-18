import LTG

import Control.Monad
import Control.Monad.State
import qualified Data.Vector.Mutable as MV
import System.IO

main :: IO ()
main = runLTG ltgMain

ltgMain :: LTG ()
ltgMain = do
  forever $ do
    I $> 255
    s <- get
    let vs = vital (myState s)
    vitals <- mapM (liftIO . MV.read vs) [0..255]

    liftIO $ hPutStrLn stderr $ "my vital list " ++ show vitals

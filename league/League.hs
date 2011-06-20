{-# OPTIONS -Wall #-}

module League
    (
     AI(..), Match(..), port,
       aiIndex, ais, aiSize
    ) where

import Data.Char
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO.Unsafe

data AI = AI {command :: String}
          deriving (Eq, Ord, Show, Read)
data Match = Match {p0::AI, p1::AI, score0::Int, 
                    alive0 :: Int, alive1 :: Int,
                    turn :: Int}
             deriving (Eq, Ord, Show, Read)


port :: Int
port = 0x9b9b

aiIndex :: AI -> Maybe Int
aiIndex ai = V.findIndex (==ai) ais 

aiSize :: Int
aiSize = V.length ais

ais :: Vector AI
ais = unsafePerformIO $ do
        ps <- fmap (map AI . filter good . lines) $ readFile "participants_global.txt"     
        return $ V.fromList ps
    where
      good line
          | all isSpace line                     = False
          | (dropWhile isSpace line) !! 0 == '#' = False
          | True                                 = True

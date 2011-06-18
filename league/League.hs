{-# OPTIONS -Wall #-}

module League
    (
     AI(..), Match(..), port,
       aiIndex, ais, aiSize
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO.Unsafe

data AI = AI {command :: String}
          deriving (Eq, Ord, Show, Read)
data Match = Match {p0::AI, p1::AI, score {- of P0 -} ::Int}
             deriving (Eq, Ord, Show, Read)

makeAI :: String -> AI
makeAI = AI

port :: Int
port = 0x9b9b

aiIndex :: AI -> Int
aiIndex ai = case V.findIndex (==ai) ais of
               Just n -> n
               Nothing -> error $ "AI not found: " ++ show ai

aiSize :: Int
aiSize = V.length ais

ais :: Vector AI
ais = unsafePerformIO $ do
        ps <- fmap (map makeAI . lines) $ readFile "participants.txt"     
        return $ V.fromList ps
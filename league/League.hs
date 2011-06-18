{-# OPTIONS -Wall #-}

module League
    (
     AI(..), port
    ) where

data AI = AI {command :: String}

port :: Int
port = 0x9b9b

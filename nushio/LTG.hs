{-# OPTIONS -Wall #-}
module LTG(Card(..), cards, right, left, ($<), ($>), (#)) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList)
import System.IO

infixl 3 #
(#) :: Text -> Text -> Text
(#) = T.append


data Card = I | Zero | Succ | Dbl | Get | Put | S | K | Inc | Dec | Attack | Help | Copy | Revive | Zombie

cards :: Vector Card
cards = fromList [I , Zero , Succ , Dbl , Get , Put , S , K , Inc , Dec , Attack , Help , Copy , Revive , Zombie]

cardName :: Card -> Text
cardName I       = pack "I"       
cardName Zero    = pack "zero"    
cardName Succ    = pack "succ"    
cardName Dbl     = pack "dbl"     
cardName Get     = pack "get"     
cardName Put     = pack "put"     
cardName S       = pack "S"       
cardName K       = pack "K"       
cardName Inc     = pack "inc"     
cardName Dec     = pack "dec"     
cardName Attack  = pack "attack"  
cardName Help    = pack "help"    
cardName Copy    = pack "copy"    
cardName Revive  = pack "revive"    
cardName Zombie  = pack "zombie"  

infix 1 $<
infix 1 $>

right, ($<) :: Int -> Card -> IO ()
right s c = do
  T.putStr $ T.unlines [pack "2", pack (show s), cardName c]
  hFlush stdout

($<) = right
  

left, ($>) :: Card -> Int -> IO ()
left c s = do
  T.putStr $ T.unlines [pack "1", cardName c, pack (show s)]
  hFlush stdout

($>) = left

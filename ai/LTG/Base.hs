module LTG.Base (
  Card(..),
  cards,
  cardName,
  ) where

import Data.Vector (Vector, fromList)

data Card = I | Zero | Succ | Dbl | Get | Put | S | K | Inc | Dec | Attack | Help | Copy | Revive | Zombie

cards :: Vector Card
cards = fromList [I , Zero , Succ , Dbl , Get , Put , S , K , Inc , Dec , Attack , Help , Copy , Revive , Zombie]

cardName :: Card -> String
cardName I       = "I"       
cardName Zero    = "zero"    
cardName Succ    = "succ"    
cardName Dbl     = "dbl"     
cardName Get     = "get"     
cardName Put     = "put"     
cardName S       = "S"       
cardName K       = "K"       
cardName Inc     = "inc"     
cardName Dec     = "dec"     
cardName Attack  = "attack"  
cardName Help    = "help"    
cardName Copy    = "copy"    
cardName Revive  = "revive"    
cardName Zombie  = "zombie"  

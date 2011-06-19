module LTG.Base (
  Card(..),
  cards,
  cardName,
  nameToCard,
  ) where

import Data.Vector (Vector, fromList)

data Card = I | Zero | Succ | Dbl | Get | Put | S | K | Inc | Dec | Attack | Help | Copy | Revive | Zombie
          deriving (Eq, Show)
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
nameToCard :: String -> Card 
nameToCard "I"        =  I       
nameToCard "zero"     =  Zero    
nameToCard "succ"     =  Succ    
nameToCard "dbl"      =  Dbl     
nameToCard "get"      =  Get     
nameToCard "put"      =  Put     
nameToCard "S"        =  S       
nameToCard "K"        =  K       
nameToCard "inc"      =  Inc     
nameToCard "dec"      =  Dec     
nameToCard "attack"   =  Attack  
nameToCard "help"     =  Help    
nameToCard "copy"     =  Copy    
nameToCard "revive"   =  Revive  
nameToCard "zombie"   =  Zombie  
nameToCard x          =  error $ "unknown card name : " ++ x

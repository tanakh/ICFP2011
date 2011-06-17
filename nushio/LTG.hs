{-# OPTIONS -Wall #-}
module LTG(Card(..), right, left, ($<), ($>)) where

import System.IO

data Card = I | Zero | Succ | Dbl | Get | Put | S | K | Inc | Dec | Attack | Help | Copy | Revive | Zombie

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

infix 1 $<
infix 1 $>

right, ($<) :: Int -> Card -> IO ()
right s c = do
  putStrLn "2"
  print s
  putStrLn $ cardName c
  hFlush stdout

($<) = right
  

left, ($>) :: Card -> Int -> IO ()
left c s = do
  putStrLn "1"
  putStrLn $ cardName c
  print s
  hFlush stdout

($>) = left

{-# OPTIONS -Wall #-}
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import Prelude hiding (show)
import qualified Prelude
import System.Environment
import System.IO
import System.Random
import System.Posix.Unistd

instance Num Text where
  (+) = Text.append
  (*)    = error "don't multiply text"
  negate = error "don't negate text"
  abs    = error "don't abs text"
  signum = error "don't signum text"
  fromInteger = error "don't fromInteger text"
  
show :: Show a => a -> Text  
show = pack . Prelude.show
  

type World = Vector (Vector (Maybe Creature))

width, height :: Num a => a
width  = 12
height = 12


data Color = White | Red | Blue | Pink | Black | Yellow  
data Race  = 
  Incubator  |
  Girl       |
  Adult      |
  PuellaMagi |
  Familiar   |
  Witch   
  
{- gakkoude narattayone -}  
eat :: Race -> Race -> Maybe Float
Witch      `eat` Adult     = Just 0.1
Witch      `eat` Girl      = Just 0.1 
Witch      `eat` Incubator = Just 0 
Familiar   `eat` Adult     = Just 0.5 
Familiar   `eat` Girl      = Just 0.5  
Familiar   `eat` Incubator = Just 0
PuellaMagi `eat` Witch     = Just 5.0  
PuellaMagi `eat` Familiar  = Just 0
Incubator  `eat` Incubator = Just 0 -- ep8 spoiler warning!
eat _ _ = Nothing

data Creature = Creature {color :: Color, race :: Race, hp :: Float} 

kyubey :: Creature
kyubey = Creature White Incubator 1

drawCreature :: Int -> Int -> Maybe Creature -> Text
drawCreature ix iy cell = 
  case cell of
    Nothing -> pack ""
    Just (Creature color' race' hp') -> Text.unwords cmds
      where
        x = (fromIntegral ix + 1) / (width +1::Float)
        y = (fromIntegral iy + 1) / (height+1::Float)
        cmds = [pack "draw",show x, show y, pack "0.5", fn, 
                   pack "1", str]
        (fn, str) = case color' of 
          White -> (pack "0-0.png", pack "keiyaku!")
          _ -> (pack "1-0.png", pack "uze-")




map2 :: (a -> b) -> Vector (Vector a) -> Vector (Vector b)
map2 = Vector.map . Vector.map 

imap2 :: (Int -> Int -> a -> b) -> Vector (Vector a) -> Vector (Vector b)
imap2 f = Vector.imap (\iy -> Vector.imap (\ix -> f ix iy)) 

foldl2 :: (a -> a -> a) -> Vector (Vector a) -> a
foldl2 f = Vector.foldl1 f . Vector.map (Vector.foldl1 f)

write2 :: Int -> Int -> a -> Vector (Vector a) -> Vector (Vector a) 
write2 ix iy val vv = vv//[(iy, (vv!iy)//[(ix, val)])]

initWorld :: World
initWorld = 
  Vector.replicate height (Vector.replicate width Nothing)

main :: IO ()
main = do
  args <- getArgs
  if length args > 0 then return ()
  else main' initWorld $ {- enkan -} cycle {- kotowari -}
       [generate, react, wander]


type Evolution = World -> IO World
generate , react, wander :: Evolution
generate world = do
  ix <- randomRIO (0, width-1)
  iy <- randomRIO (0, height-1)
  case world ! iy ! ix of
    Nothing -> return $ write2 ix iy (Just kyubey) world 
    _       -> return world
      
react = return 
wander = return

  
  
main' :: World -> [Evolution] -> IO ()
main' _ [] = undefined
main' world (f:fs) = do
  Text.putStrLn $ foldl2 sep $ imap2 drawCreature world
  Text.putStrLn $ pack "flush"
  hFlush stdout  
  usleep 10000
  world' <- f world
  main' world' fs
    where
      sep a b = a + pack "\n" + b

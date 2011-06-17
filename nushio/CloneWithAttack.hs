{-# OPTIONS -Wall #-}
import LTG hiding(($<), ($>))
import System.Environment
import System.Random

skip :: IO ()
skip = do
  _ <- getLine
  _ <- getLine
  _ <- getLine
  return ()

($<) x y = do
  right x y
  skip
  
($>) x y = do
  left x y
  skip

-- Love me do
apply0 :: Int -> IO ()
apply0 field = do
  K $> field
  S $> field
  field $< Get
  field $< Zero

clear :: Int -> IO()
clear field = do
  Zero $> field

num :: Int -> Int -> IO ()
num field n = do
  clear field
  field $< Zero
  num_iter n
  where
    num_iter 0 = do
      return ()
    num_iter 1 = do
      Succ $> field
    num_iter q = do
      num_iter (q `div` 2)
      Dbl $> field
      if q `mod` 2 == 0 
        then return ()
        else (Succ $> field) >> (return ())

-- Example: attack 3 4 10
attack from to value = do
  -- v[1] <- (Attack from)
  num 1 from
  Attack $> 1
  -- v[0] <- to
  num 0 to
  -- v[1] <- apply v[1] v[0]
  apply0 1
  -- v[0] <- value
  num 0 value
  -- v[1] <- apply v[1] v[0]
  apply0 1

copyto :: Int -> Int -> IO ()
copyto fromfield tofield = do
  num tofield fromfield
  Get $> tofield
  
  
{- 
ゾンビは相手ターンに次のコードを実行する
\I -> zombie 0 (copy idxopp)
このゾンビは自分ターンに次のコードを実行するようにzombieを挿入する
\I -> zombie 0 (get idxprop)

これを実現するコードは以下のように表現される
S (K (zombie 0)) (K (S (K copy/get) (K id)))
-}

incubate :: Card -> Int -> Int -> Int -> IO()
incubate card f fwork idx = do
  -- (S (K (zombie 0)))
  clear f
  f $< Zero
  Zombie $> f
  K $> f
  S $> f
  -- (S (K <card>) (K <idx>)))
  num 0 idx
  K $> 0
  clear fwork
  fwork $< card
  K $> fwork
  S $> fwork
  apply0 fwork
  copyto fwork 0
  -- Done
  apply0 f

attackCode :: Int -> IO ()
attackCode f = do
  clear f
  r <- randomRIO (0, 2 :: Int)
  target <- randomRIO (0, 255 :: Int)
  drainfrom <- randomRIO (0, 255 :: Int)
  value <- randomRIO (0,13 :: Int)
  let q = 2 ^ value
  let arity3 card = do
        num f target
        card $> f
        num 0 drainfrom
        apply0 f
        K $> f
        S $> f
        num 0 q
        K $> 0
        apply0 f
  let arity1 card = do
        num 0 target
        K $> 0
        f $< card
        K $> f
        S $> f
        apply0 f
  case r of
    0 ->
      arity3 Help
    1 -> 
      arity3 Attack
    2 ->
      arity1 Dec
    3 ->
      arity1 Inc

{-
append attack process
S (zombie-process) (K attack-process) 
where (attack-process) is help, attack, dec, inc, etc.
-}
incubateWithAttack :: Int -> Int -> Int -> IO ()
incubateWithAttack workf idxopp idxprop = do
  incubate Copy idxprop workf idxopp
  incubate Get idxopp workf idxprop
  S $> idxprop
  attackCode workf
  copyto workf 0
  apply0 idxprop
  copyto idxopp 0
  0 $< I
  
-- inject zombie that clone itself
injectOctavia :: Int -> Int -> Int -> IO ()
injectOctavia workf idxopp idxprop = do
  incubate Copy idxprop workf idxopp
  incubate Get idxopp workf idxprop
  -- Clarissa is ready
  -- Zombie 0 (get idxopp)
  clear workf
  workf $< Zero
  Zombie $> workf
  copyto idxprop 0
  -- Inject!
  apply0 workf

sittingDuck :: IO()
sittingDuck = do
  I $> 0
  sittingDuck
  
attackloop :: Int -> Int -> Int -> IO()
attackloop v k s = do
  attack v k 8192
  attack (v+1) k 8192
  -- inject_sayasaya (v+2) (v+3) s 8192
  if v > 240 
    then attackloop 2 (k+1) 2
    else attackloop (v + 4) (k+1) (s+1)

main :: IO()
main = do
  [arg] <- getArgs
  let b = (read arg :: Int) -- 0: Sente, 1: Gote
  if b == 1 then skip else return ()
  attack 3 0 8192
  attack 255 0 10000 -- commit suicide
  incubateWithAttack 5 6 7
  sittingDuck




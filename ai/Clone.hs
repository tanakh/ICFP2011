{-# OPTIONS -Wall #-}
import LTG 
import LTG.SoulGems
  

{- 
ゾンビは相手ターンに次のコードを実行する
\I -> zombie 0 (copy idxopp)
このゾンビは自分ターンに次のコードを実行するようにzombieを挿入する
\I -> zombie 0 (get idxprop)

これを実現するコードは以下のように表現される
-}
incubate :: Card -> Int -> Int -> Int -> LTG()
incubate card f fwork idx = do
  -- (\_ -> <card> <idx>)
  clear fwork
  fwork $< card
  num f idx
  lazyApply fwork f
  
  -- (zombie 0)
  clear f
  f $< Zero
  Zombie $> f
  
  -- (\I -> (zombie 0) ((\_ -> <card> <idx>) I))
  compose f fwork

{-
"Helpful"なゾンビ行動
-}

-- S Help I x = Help x x
setHelpfulCode :: Int -> LTG()
setHelpfulCode f = do
  clear f
  f $< S
  f $< Help
  f $< I

createHelpfulZombieCode :: Int -> Int -> Int -> Int -> Int -> LTG()
createHelpfulZombieCode f fwork fwork2 value order = do
  -- v[f] <- (\x -> (\y -> Help y y) (Copy x))
  setHelpfulCode f
  clear fwork
  fwork $< Copy
  compose f fwork
  -- v[f] <- (\_ -> v[f] order value)
  num fwork order
  num fwork2 value
  lazyApply2 f fwork fwork2

{-
改良版ゾンビ。相手ターンに次のコードを実行する。
\I -> S (zombie 0 (copy idxopp)) (\x -> (\y -> Help y y) (Copy x))
-}
helpfulIncubate :: Int -> Int -> Int -> Int -> LTG()
helpfulIncubate f fwork fwork2 idx = do
  createHelpfulZombieCode fwork2 fwork f 8192 4
  incubate Copy f fwork idx 
  S $> f
  copyTo0 fwork2
  apply0 f
  
-- inject zombie that clone itself
injectOctavia :: Int -> Int -> Int -> LTG ()
injectOctavia workf idxopp idxprop = do
  helpfulIncubate idxprop workf 5 idxopp
  incubate Get idxopp workf idxprop
  -- Clarissa is ready
  copyTo 0 idxopp
  num 4 0
  -- inject!
  0 $< I
  dloop (255 :: Int)
  where 
    dloop 0 = sittingDuck
    dloop x = do 
      Succ $> 4
      dloop (x-1)

sittingDuck :: LTG()
sittingDuck = do
  I $> 0
  sittingDuck
  
attackloop :: Int -> Int -> Int -> LTG()
attackloop v k s = do
  attack v k 8192
  attack (v+1) k 8192
  -- inject_sayasaya (v+2) (v+3) s 8192
  if v > 240 
    then attackloop 2 (k+1) 2
    else attackloop (v + 4) (k+1) (s+1)

introAttack :: LTG ()
introAttack = do
  3      $< Zero
  Succ   $> 3
  4      $< Zero
  Succ   $> 4
  Succ   $> 4
  Attack $> 3
  Attack $> 4
  -- 7

  0    $< Zero
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  Dbl  $> 0
  Succ $> 0
  -- v[0] = 255; 16

  apply0 3
  apply0 4
  -- 8

  Dbl  $> 0
  Dbl  $> 0
  Dbl  $> 0
  Dbl  $> 0
  Dbl  $> 0
  -- 5

  apply0 3
  apply0 4
  -- 8
  
main :: IO ()
main = runLTG $ do
  introAttack
  attack 3 0 8192
  attack 255 0 10000
  injectOctavia 1 2 3
  sittingDuck
  
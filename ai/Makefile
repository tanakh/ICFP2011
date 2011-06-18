all: Test \
	 SittingDuck  Random

Random: Random.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Random.hs

SittingDuck: SittingDuck.hs  LTG.hs LTG/*.hs
	ghc --make -O3 SittingDuck.hs

Test: Test.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Test.hs



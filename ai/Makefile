all: Test \
	SittingDuck  Random \
	CompactSayaka

CompactSayaka: CompactSayaka.hs  LTG.hs LTG/*.hs
	ghc --make -O3 CompactSayaka.hs

Random: Random.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Random.hs

SittingDuck: SittingDuck.hs  LTG.hs LTG/*.hs
	ghc --make -O3 SittingDuck.hs

Test: Test.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Test.hs



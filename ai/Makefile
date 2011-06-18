all: Test \
	SittingDuck  Random \
	CompactSayaka NonZeroCompactSayaka \
	Watcher

CompactSayaka: CompactSayaka.hs LTG.hs LTG/*.hs
	ghc --make -O3 CompactSayaka.hs

NonZeroCompactSayaka: NonZeroCompactSayaka.hs LTG.hs LTG/*.hs
	ghc --make -O3 NonZeroCompactSayaka.hs

Random: Random.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Random.hs

SittingDuck: SittingDuck.hs  LTG.hs LTG/*.hs
	ghc --make -O3 SittingDuck.hs

Test: Test.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Test.hs

Watcher: Watcher.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Watcher.hs


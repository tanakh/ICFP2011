all: Test \
	SittingDuck  Random \
	CompactSayaka NonZeroCompactSayaka \
	KuroMadoka Kyouko \
	Watcher

CompactSayaka: CompactSayaka.hs LTG.hs LTG/*.hs
	ghc --make -O3 CompactSayaka.hs

NonZeroCompactSayaka: NonZeroCompactSayaka.hs LTG.hs LTG/*.hs
	ghc --make -O3 NonZeroCompactSayaka.hs

KuroMadoka: KuroMadoka.hs LTG.hs LTG/*.hs
	ghc --make -O3 KuroMadoka.hs

Kyouko: Kyouko.hs LTG.hs LTG/*.hs
	ghc --make -O3 Kyouko.hs

Random: Random.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Random.hs

SittingDuck: SittingDuck.hs  LTG.hs LTG/*.hs
	ghc --make -O3 SittingDuck.hs

Test: Test.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Test.hs

Watcher: Watcher.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Watcher.hs


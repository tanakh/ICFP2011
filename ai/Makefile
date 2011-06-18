all: Test \
	SittingDuck  Random \
	CompactSayaka NonZeroCompactSayaka \
	Watcher \
	SayakaRevive \
	KuroMadoka Kyouko KyoukoFA \
	Clone \
	FastZombie

CompactSayaka: CompactSayaka.hs LTG.hs LTG/*.hs
	ghc --make -O3 CompactSayaka.hs

NonZeroCompactSayaka: NonZeroCompactSayaka.hs LTG.hs LTG/*.hs
	ghc --make -O3 NonZeroCompactSayaka.hs

KuroMadoka: KuroMadoka.hs LTG.hs LTG/*.hs
	ghc --make -O3 KuroMadoka.hs

Kyouko: Kyouko.hs LTG.hs LTG/*.hs
	ghc --make -O3 Kyouko.hs

KyoukoFA: KyoukoFA.hs LTG.hs LTG/*.hs
	ghc --make -O3 KyoukoFA.hs

Random: Random.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Random.hs

SayakaRevive: SayakaRevive.hs  LTG.hs LTG/*.hs
	ghc --make -O3 SayakaRevive.hs


SittingDuck: SittingDuck.hs  LTG.hs LTG/*.hs
	ghc --make -O3 SittingDuck.hs

Test: Test.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Test.hs

Watcher: Watcher.hs  LTG.hs LTG/*.hs
	ghc --make -O3 Watcher.hs

Clone: Clone.hs LTG.hs LTG/*.hs
	ghc --make -O3 Clone.hs

FastZombie: FastZombie.hs LTG.hs LTG/*.hs
	ghc --make -O3 FastZombie.hs

all: SittingDuck BCombi 255 Random UnstableSayaka SayakaTestWithRandom

SittingDuck: SittingDuck.hs LTG.hs
	ghc --make -O3 SittingDuck.hs

BCombi: BCombi.hs LTG.hs
	ghc --make -O3 BCombi.hs

255: 255.hs LTG.hs
	ghc --make -O3 255.hs
SayakaTestWithRandom: SayakaTestWithRandom.hs LTG.hs
	ghc --make -O3 SayakaTestWithRandom.hs

UnstableSayaka: UnstableSayaka.hs LTG.hs
	ghc --make -O3 UnstableSayaka.hs

Random: Random.hs LTG.hs
	ghc --make -O3 Random.hs


clean:
	rm -f *~

clobber:
	rm -f *~ *.hi *.o



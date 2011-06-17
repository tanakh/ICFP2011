all: SittingDuck BCombi 255 Random

SittingDuck: SittingDuck.hs LTG.hs
	ghc --make SittingDuck.hs

BCombi: BCombi.hs LTG.hs
	ghc --make BCombi.hs

255: 255.hs LTG.hs
	ghc --make 255.hs

Random: Random.hs LTG.hs
	ghc --make Random.hs


clean:
	rm -f *~

clobber:
	rm -f *~ *.hi *.o



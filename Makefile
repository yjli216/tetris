all:
	ghc --make -O -threaded parser.hs
clean:
	rm *.o *.hi parser
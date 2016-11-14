all:
	ghc --make -O -threaded tetris.hs
clean:
	rm *.o *.hi tetris
all:
	ghc -o data data.hs
clean:
	rm *.o *.hi data
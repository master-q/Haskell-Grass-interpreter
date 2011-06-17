grass: grass.hs
	ghc --make -Wall grass.hs

lint:
	hlint grass.hs

test: grass
	stdiochk -n ./grass testdata

clean:
	rm -rf grass grass.o grass.hi
	rm -rf *~
	rm -rf */*~

.PHONY: lint clean

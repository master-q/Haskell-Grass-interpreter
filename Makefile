GrassCommand: GrassCommand.hs
	ghc --make -Wall GrassCommand.hs

lint:
	hlint GrassCommand.hs Grass.hs

test: GrassCommand
	stdiochk -n ./GrassCommand testdata

clean:
	rm -rf GrassCommand
	rm -rf *.hi *.o
	rm -rf *~
	rm -rf */*~

.PHONY: lint clean

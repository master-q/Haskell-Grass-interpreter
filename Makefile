all: GrassCommand GrassFs

GrassCommand: GrassCommand.hs Grass.hs
	ghc --make -Wall GrassCommand.hs

GrassFs: GrassFs.hs Grass.hs
	ghc -threaded --make -Wall GrassFs.hs

lint:
	hlint -c GrassCommand.hs GrassFs.hs Grass.hs

test: test_command

test_command: GrassCommand
	stdiochk -n ./GrassCommand testdata

test_fs: GrassFs
	mkdir -p mount
	./GrassFs mount
	ls mount
	cat mount/grassvm
	fusermount -u mount
	rm -rf mount

mount: GrassFs
	mkdir -p mount
	./GrassFs mount

umount:
	-fusermount -u mount
	rm -rf mount

clean:
	rm -rf GrassCommand GrassFs
	rm -rf *.hi *.o
	rm -rf *~
	rm -rf */*~
	-fusermount -u mount
	rm -rf mount

.PHONY: lint clean mount umount

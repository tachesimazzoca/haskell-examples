all: build

.PHONY: build
build:
	@cabal configure --prefix=`readlink -f .` --user --enable-tests
	@cabal build

.PHONY: test 
test:
	@cabal test

.PHONY: checkstyle
checkstyle:
	@find src -name *.hs | xargs ~/.cabal/bin/hlint

.PHONY: docs
docs:
	@cabal haddock --haddock-options='--source-module=https://github.com/tachesimazzoca/haskell-examples/blob/master/src/modules/%{MODULE/.//}.hs'

.PHONY: install
install:
	@cabal install --prefix=`readlink -f .` --user

.PHONY: clean
clean:
	@cabal clean && rm -rf lib share

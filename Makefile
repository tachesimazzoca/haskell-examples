all: build

.PHONY: build
build:
	@cabal configure --prefix=`readlink -f .` --user --enable-tests
	@cabal build

.PHONY: install
install:
	@cabal install --prefix=`readlink -f .` --user

.PHONY: test 
test:
	@cabal test

.PHONY: clean
clean:
	@cabal clean && rm -rf lib share

.PHONY: docs
docs:
	@cabal haddock 

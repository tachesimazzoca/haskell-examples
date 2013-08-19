all: build

.PHONY: build
build:
	@cabal-dev install --only-dependencies --enable-tests
	@cabal-dev configure --enable-tests
	@cabal-dev build

.PHONY: test 
test:
	@cabal-dev test

.PHONY: checkstyle
checkstyle:
	@find src/modules -name *.hs | xargs hlint

.PHONY: docs
docs:
	@cabal-dev haddock --haddock-options='--source-module=https://github.com/tachesimazzoca/haskell-examples/blob/master/src/modules/%{MODULE/.//}.hs'

.PHONY: clean
clean:
	@cabal-dev clean

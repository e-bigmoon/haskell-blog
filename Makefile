.PHONY: dev
dev:
			stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -qg -RTS"

.PHONY: fmt
fmt:
			$(eval FILES := $(shell find app -type f -name "*.hs"))
			ormolu -o -XTypeApplications --mode inplace $(FILES)

.PHONY: cabal-fmt
cabal-fmt:
			cabal-fmt -i bigmoon-haskellers-blog.cabal

.PHONY: lint
lint:
			$(eval FILES := $(shell find app -type f -name "*.hs"))
			hlint $(FILES)

.PHONY: watch
watch:
			stack run watch

.PHONY: pedantic
pedantic:
			stack clean && stack test --fast --pedantic --no-run-tests --ghc-options "-j4 +RTS -A128m -n2m -qg -RTS"

.PHONY: clean
clean:
			stack run clean
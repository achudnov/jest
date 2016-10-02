GRAPHCASES=$(wildcard test-data/cfg/*.js)

all:
	stack build
test:
	stack test
debug-test:
	rm -f dist/test/*.js
	stack test jest --test-arguments "--jse-use-tong --jse-dump-source --jse-dump-prefix test-dump/ --jsqc-isolate --jsqc-prefix test-dump/"
setup:
	cabal sandbox init
	cabal install --only-dependencies
	cabal configure
dev-setup:
	cabal sandbox init
	cabal sandbox add-source ../language-ecmascript/
	cabal sandbox add-source ../language-ecmascript-analysis/
	cabal sandbox add-source ../haxy/
	cabal sandbox add-source ../hs-webdriver/
	cabal sandbox add-source ../http-encodings/
	cabal sandbox add-source ../jespresso/
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests
	opam update
	opam install flowtype
clean:
	cabal clean
	cabal sandbox delete
drawgraphs:
	$(foreach case, $(GRAPHCASES), stack exec graphgen $(case) && dot -Tsvg $(addsuffix .dot, $(basename $(case))) > $(addsuffix .svg, $(basename $(case))) &&) echo "Done"
cfggencase:
	dist/build/randjs/randjs -s=7 > test-data/cfg/$(shell pwgen -1).js
rtsdebug: all
	stack exec rtsdebug rtsrepl > test-dump/repl.js
	stack exec rtsdebug rtsdebugger > test-dump/debug.html
rtsrepl: rtsdebug
	js test-dump/repl.js
rtsdebugger: rtsdebug
	firefox dist/build/rtsdebug/debug.html
printnotes:
	find src/ -name \*.hs | xargs sed -n -f ./ghc-note-extract.sed
	find rts/ -name \*.js | xargs sed -n -f ./js-note-extract.sed
benchmark: all
	cabal run bench bench/benchlist bench/bench.js
	nodejs bench/bench.js | jq '[.[] | {instrumented: .instrumented.mean, original: .uninstrumented.mean, slowdown: (.instrumented.mean / .uninstrumented.mean)}]' > bench/benchresults.json
benchmark-opt: all
	dist/build/bench/bench bench/benchlist bench/bench.js --oo
	nodejs bench/bench.js | jq '[.[] | {instrumented: .instrumented.mean, original: .uninstrumented.mean, slowdown: (.instrumented.mean / .uninstrumented.mean)}]' > bench/benchresults.json
benchmark-analysis: all
	cabal run bench bench/benchlist bench/bench.js
	nodejs --trace-opt --trace-deopt --prof bench/bench.js > bench/bench-tracing 
benchmark-opt-analysis: all
	dist/build/bench/bench bench/benchlist bench/bench.js --oo
	nodejs --trace-opt --trace-deopt --prof bench/bench.js > bench/bench-tracing

.PHONY: all
all: examples

.PHONY: examples
examples: ../publish/index.html ../publish/assets

../publish/index.html: $(shell find . -type f -name '*.elm')
	cd examples/les-bonnes-choses && elm-make --warn src/Main.elm --output ../../../publish/index.html

../publish/assets: $(shell find examples/les-bonnes-choses/assets -type f)
	mkdir -p ../publish/assets && cp -r examples/les-bonnes-choses/assets ../publish/

.PHONY: clean
clean:
	rm -rf elm-stuff/build-artifacts examples/les-bonnes-choses/elm-stuff/build-artifacts && rm ../publish/index.html

.PHONY: reactor
reactor:
	cd examples/les-bonnes-choses && elm-reactor

.PHONY: serve
serve:
	cd ../publish && python2 -m SimpleHTTPServer

.PHONY: watch
watch:
	bash ./bin/watch.sh

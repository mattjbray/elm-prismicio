.PHONY: all
all: examples

.PHONY: examples
examples:
	cd examples/les-bonnes-choses && elm-make --warn src/Main.elm --output ../../../publish/index.html

.PHONY: clean
clean:
	rm -rf elm-stuff/build-artifacts examples/les-bonnes-choses/elm-stuff/build-artifacts

.PHONY: serve
serve:
	cd examples/les-bonnes-choses && elm-reactor

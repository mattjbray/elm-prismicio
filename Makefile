.PHONY: all
all: docs examples

.PHONY: examples
examples: les-bonnes-choses

.PHONY: les-bonnes-choses
les-bonnes-choses: ../publish/js/app.js ../publish/index.html les-bonnes-choses-assets

../publish/js/app.js: $(shell find . -type f -name '*.elm')
	mkdir -p ../publish/js && \
	cd examples/les-bonnes-choses && \
	elm-make --warn src/Main.elm --output ../../../publish/js/app.js

../publish/index.html: examples/les-bonnes-choses/index.html
	mkdir -p ../publish && cp $< $@

les-bonnes-choses-assets: $(subst examples/les-bonnes-choses,../publish,$(shell find examples/les-bonnes-choses/assets -type f))

../publish/assets/%: examples/les-bonnes-choses/assets/%
	mkdir -p $(dir $@) && cp $< $@

.PHONY: clean
clean:
	rm -rf elm-stuff/build-artifacts \
	       examples/les-bonnes-choses/elm-stuff/build-artifacts \
	       ../publish/index.html \
	       ../publish/js/app.js \
	       ../publish/assets \
	       documentation.json

.PHONY: reactor
reactor:
	cd examples/les-bonnes-choses && \
	elm-reactor

.PHONY: serve
serve:
	cd ../publish && \
	python2.7 -m SimpleHTTPServer

.PHONY: watch
watch:
	bash ./bin/watch.sh

.PHONY: docs
docs: documentation.json

documentation.json: src/Prismic.elm
	elm-make --docs=documentation.json

.PHONY: all
all: documentation.json examples

.PHONY: examples
examples: sample-website

.PHONY: sample-website
sample-website: examples/website/app.js

examples/website/app.js: $(shell find . -type f -name '*.elm')
	cd examples/website && \
	elm-make --warn src/Main.elm --output app.js

documentation.json: $(shell find ./src -type f -name '*.elm')
	elm-make --docs=documentation.json

.PHONY: clean
clean:
	rm -rf elm-stuff/build-artifacts \
	       examples/website/elm-stuff/build-artifacts \
	       examples/website/app.js \
	       documentation.json

.PHONY: publish-example
publish-example: sample-website
	mkdir -p publish
	cp examples/website/app.js publish/app.js
	cp examples/website/index.html publish/index.html
	git stash
	git checkout gh-pages
	git pull
	cp publish/app.js .
	cp publish/index.html .
	git add app.js index.html
	git commit -m "Update example."
	git push origin gh-pages
	git checkout -
	rm -rf publish
	git stash pop

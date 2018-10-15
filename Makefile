#!/bin/bash
.PHONY: build
.SUFFIXES: .elm .js

build:
	$(MAKE) -C example build


gh-pages: build
	git branch -D gh-pages 2>/dev/null || true
	git branch -D draft 2>/dev/null || true
	git push origin :gh-pages
	git checkout -b draft
	git add -f example/main.js
	git commit -am "Deploy on gh-pages"
	git subtree push --prefix example origin gh-pages

#!/bin/bash
.PHONY: build
.SUFFIXES: .elm .js

build:
	elm make --warn example/Main.elm --output=example/main.js

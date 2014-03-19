all: lib test

deps:
	git clone git@github.com:purescript/purescript-quickcheck.git

lib:
	mkdir -p js/
	psc \
	  src/Data/Map.purs.hs \
	  src/Data/Set.purs.hs \
	  -o js/lib.js \
	  -e js/lib.externs \
	  --module Data.Map --module Data.Set --tco --magic-do

test:
	mkdir -p js/
	psc \
	  src/Data/Map.purs.hs \
	  src/Data/Set.purs.hs \
	  tests/tests.purs.hs \
	  purescript-quickcheck/src/Test/QuickCheck.purs.hs\
	  -o js/tests.js \
	  --main --module Main --module Prelude --tco --magic-do

docs: docs/Map.md docs/Set.md

docs/Map.md:
	docgen src/Data/Map.purs.hs > docs/Map.md

docs/Set.md:
	docgen src/Data/Set.purs.hs > docs/Set.md


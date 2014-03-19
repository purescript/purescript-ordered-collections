all: lib test

deps:
	git clone git@github.com:purescript/purescript-quickcheck.git

lib:
	psc \
	  src/Data/Map.purs.hs \
	  src/Data/Set.purs.hs \
	  -o js/lib.js \
	  --module Data.Map --module Data.Set --tco --magic-do

test:
	psc \
	  src/Data/Map.purs.hs \
	  src/Data/Set.purs.hs \
	  tests/tests.purs.hs \
	  purescript-quickcheck/src/Test/QuickCheck.purs.hs\
	  -o js/tests.js \
	  --main --module Main --module Prelude --tco --magic-do

docs: docs/README.md

docs/README.md:
	docgen src/Data/Map.purs.hs > docs/README.md


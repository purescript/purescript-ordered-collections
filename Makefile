all: lib test

deps:
	git clone git@github.com:purescript/purescript-quickcheck.git

lib:
	mkdir -p js/Data/
	psc src/Data/Map.purs.hs \
	  -o js/Data/Map.js \
	  -e js/Data/Map.e.purs.hs \
	  --module Data.Map --tco --magic-do

test:
	mkdir -p js/
	psc src/Data/Map.purs.hs \
	  purescript-quickcheck/src/Test/QuickCheck.purs.hs\
	  tests/tests.purs.hs \
	  -o js/tests.js \
	  --main --module Main --tco --magic-do

all: lib test

lib:
	mkdir -p js/Data/
	psc src/Data/Map.purs.hs \
	  -o js/Data/Map.js \
	  -e js/Data/Map.e.purs.hs \
	  --module Data.Map --tco --magic-do

test:
	mkdir -p js/
	psc src/Data/Map.purs.hs \
	  examples/test.purs.hs \
	  -o js/test.js \
	  --main --module Main --tco --magic-do

#!/bin/bash
stack exec -- haddock --html src/*.hs src/ITEA/*.hs src/IT/* --hyperlinked-source --odir=docs --quickjump

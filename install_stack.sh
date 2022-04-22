#!/bin/bash
stack --local-bin-path ./bin --copy-bins build
cp ./bin/itea ./python/
cd python 
pip install .

#!/bin/bash
stack --local-bin-path ./bin --copy-bins build
cd python 
pip install .

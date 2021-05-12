#!/bin/bash
stack build --profile # --ghc-options="-fno-prof-auto"
time stack --profile exec itea config configs/itea/airfoil0.cfg -- +RTS -p -h 

#!/bin/bash
echo "fold 0"
for i in 1 2 3 4 5 6; do stack run itea configs/itea/Geographical0.cfg -- +RTS -N3; done
echo "fold 1"
for i in 1 2 3 4 5 6; do stack run itea configs/itea/Geographical1.cfg -- +RTS -N3; done
echo "fold 2"
for i in 1 2 3 4 5 6; do stack run itea configs/itea/Geographical2.cfg -- +RTS -N3; done
echo "fold 3"
for i in 1 2 3 4 5 6; do stack run itea configs/itea/Geographical3.cfg -- +RTS -N3; done
echo "fold 4"
for i in 1 2 3 4 5 6; do stack run itea configs/itea/Geographical4.cfg -- +RTS -N3; done

# towerData, tecator, Geographical

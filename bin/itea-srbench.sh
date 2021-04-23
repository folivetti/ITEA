#!/bin/bash
export LD_LIBRARY_PATH=~/.conda/envs/srbench/lib:$LD_LIBRARY_PATH:$PWD/bin
bin/itea-srbench $1 $2

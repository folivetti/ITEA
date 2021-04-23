#!/bin/bash
conda activate srbench

if [ ! -f bin/libgsl.so.0 ]; then
  cp $CONDA_PREFIX/lib/libgsl.so bin/libgsl.so.0
fi

export LD_LIBRARY_PATH=$CONDA_PREFIX/lib:$LD_LIBRARY_PATH:$PWD/bin
bin/itea-srbench $1 $2

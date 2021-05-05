#!/bin/bash
export LD_PRELOAD=/opt/intel/mkl/lib/intel64/libmkl_core.so:/opt/intel/mkl/lib/intel64/libmkl_rt.so:/opt/intel/mkl/lib/intel64/libiomp5.so:/opt/intel/mkl/lib/intel64/libmkl_sequential.so:/opt/intel/mkl/lib/intel64/libmkl_avx2.so
stack run config $1

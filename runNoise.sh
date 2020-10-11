#!/bin/bash
for i in {1..30}
do
  stack run itea config configs/noise/$1_0.cfg
  stack run itea config configs/noise/$1_0.05.cfg
  stack run itea config configs/noise/$1_0.1.cfg
  stack run itea config configs/noise/$1_0.3.cfg
  stack run itea config configs/noise/$1_1.cfg
done

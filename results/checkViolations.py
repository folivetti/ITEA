import pandas as pd
import numpy as np

from funs import *
from samples import genSamples
from violations import constraints

np.seterr(all="ignore")

datanames = ["aircraft_lift", "flow_psi", 
                "I_15_3t", "I_15_3x", 
                "I_30_5", "I_32_17",
                "I_41_16", "I_48_20",
                "I_6_20", "I_9_18", 
                "II_11_27", "II_11_28",
                "II_35_21", "II_6_15a", "III_10_19",
                "III_9_52", "Jackson211", "rocket_fuel", "WavePower"]

noises = ["0","0.05","0.1","0.3", "1"]

N = 1000000

for dataname in datanames:
    for noise in noises:
        dirname = f"{dataname}_{noise}"
        df = pd.read_csv(f"{dirname}/stats.csv")
        exprs = df.expr.values

        z = np.loadtxt(f"../../FI2POP/datasets/noise/{dirname}_train.csv", delimiter=",")
        xtrain = z[:,:-1]
        ytrain = z[:,-1]

        samples = genSamples(dataname, N)

        viol = 0
        for expr in exprs:
            ws, b   = getWeights(expr, xtrain, ytrain)
            viol   += constraints(dataname, samples, expr, ws, b)
        print(f"{dataname}, {noise}, {viol}")


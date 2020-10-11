import pandas as pd
import numpy as np
from collections import defaultdict

d = defaultdict(lambda: 100)
d['flow_stress'] = 1623

dnames = ["aircraft_lift", "flow_psi", "I_15_3t", "I_15_3x", "I_26_2", "I_29_16", "I_30_5", "I_32_17", "I_37_4", "I_41_16", "I_48_20", "I_6_20b", "I_6_20", "I_8_14", "I_9_18", "II_11_27", "II_11_28", "II_35_21", "II_6_15a", "III_10_19", "III_9_52", "Jackson211", "rocket_fuel", "WavePower", "flow_stress"]
folder = "noise/"

noises = [0, 0.05, 0.1, 0.3, 1]

for dname in dnames:
    print(dname)
    df = pd.read_csv(f'{dname}.csv', delimiter=';')
    raw = df.values
    X = raw[:,:-5]
    Y = raw[:,-5:]
    Y_05 = raw[:,-4]
    Y_1 = raw[:,-3]
    Y_3 = raw[:,-2]
    Y_1 = raw[:,-1]

    print(X[0,:])

    for i, n in enumerate(noises):
        Y_t = Y[:,i]
        Z = np.append(X, Y_t[:,None], 1)
        np.savetxt(f"{folder}{dname}_{n}_train.csv", Z[:d[dname],:], delimiter=",")
        np.savetxt(f"{folder}{dname}_{n}_test.csv", Z[d[dname]:,:], delimiter=",")

import pandas as pd

header = "Dataset,Algorithm,Train,Noise,NMSE"

datasets = ["aircraft_lift", "flow_psi", "I_15_3t", "I_15_3x", "I_26_2", "I_29_16", "I_30_5", "I_32_17", "I_37_4", "I_41_16", "I_48_20", "I_6_20b", "I_6_20", "I_8_14", "I_9_18", "II_11_27", "II_11_28", "II_35_21", "II_6_15a", "III_10_19", "III_9_52", "Jackson211", "rocket_fuel", "WavePower", "flow_stress"]

noises = [0, 0.05, 0.1, 0.3, 1]

fw = open("resultsNoise.csv","w")
fw.write(f"{header}\n")

for dname in datasets:
    for noise in noises:
        fname = f"results/{dname}_{noise}/stats.csv"
        df = pd.read_csv(fname)
        nmse_train = df.NMSE_train.median()
        nmse_test = df.NMSE_test.median()
        fw.write(f"{dname},ITEA,Y,{noise},{nmse_train}\n")
        fw.write(f"{dname},ITEA,N,{noise},{nmse_test}\n")

fw.close()


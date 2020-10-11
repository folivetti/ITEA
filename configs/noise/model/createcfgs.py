def genModel(dname, noise, knowledge):
    return f'''[Dataset]
train = datasets/noise/{dname}_{noise}_train.csv
test = datasets/noise/{dname}_{noise}_test.csv
noiseless = datasets/noise/{dname}_0_test.csv 

[Mutation]
exponents      = (-3, 3)
termlimit      = ( 1, 3)
nonzeroexps    = 1
transfunctions = FAll

[Algorithm]
npop  = 500
ngens = 500
log   = PartialLog "results/{dname}_{noise}"

{knowledge}
'''

dnames = ["aircraft_lift", "flow_psi", "I_15_3t", "I_15_3x", "I_26_2", "I_29_16", "I_30_5", "I_32_17", "I_37_4", "I_41_16", "I_48_20", "I_6_20b", "I_6_20", "I_8_14", "I_9_18", "II_11_27", "II_11_28", "II_35_21", "II_6_15a", "III_10_19", "III_9_52", "Jackson211", "rocket_fuel", "WavePower", "flow_stress"]

noises = [0, 0.05, 0.1, 0.3, 1]

for dname in dnames:
    f = open(f"model/{dname}.cfg","r")
    knowledge = f.read()
    f.close()
    for noise in noises:
        fw = open(f"{dname}_{noise}.cfg", "w")
        fw.write(genModel(dname,noise,knowledge))
        fw.close()

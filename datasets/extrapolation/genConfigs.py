algs = ["ITEA", "FI2POP"]
noises = ["0", "0.1", "0.3", "1"]


constrs = {}
constrs['I_6_20'] = '''shapes = [Image (0, Infinity), NonIncreasing 1]
domains = Just [(1 ,  3),(1 ,  3)]
varnames = ["sigma","theta"]
'''
constrs['I_9_18'] = '''shapes = [Image (0, 1e8), NonDecreasing 0, NonDecreasing 1, NonDecreasing 2, NonIncreasing 3, NonDecreasing 4, NonIncreasing 5, NonDecreasing 6, NonIncreasing 7, NonDecreasing 8] 
domains = Just [(1 ,  2),(1 ,  2),(1 ,  2),(3 ,  4),(1 ,  2),(3 ,  4),(1 ,  2),(3 ,  4),(1 ,  2)]
varnames = ["m1","m2","G","x1","x2","y1","y2","z1","z2"]
'''
constrs['I_15_3x'] = '''shapes = [Image (0, 1e8), NonDecreasing 0, NonIncreasing 2, NonIncreasing 3]
domains = Just [(5 ,  10),(1 ,  2),(3 ,  20),(1 ,  2)]
varnames = ["x","u","c","t"]
'''
constrs['I_30_5'] = '''shapes = [Image (0, Infinity), NonDecreasing 0, NonIncreasing 1, NonIncreasing 2]
domains = Just [(1, 5),(2, 5),(1, 5)]
varnames = ["lambd","d","n"]
'''
constrs['I_32_17'] = '''shapes = [Image (0, 1e4), NonDecreasing 0, NonDecreasing 1, NonDecreasing 2, NonDecreasing 3, NonDecreasing 4, NonIncreasing 5]
domains = Just [(1, 2),(1, 2),(1, 2),(1, 2),(1, 2),(3, 5)]
varnames = ["epsilon","c","Ef","r","omega","omega_0"]
'''
constrs['I_41_16'] = '''shapes = [Image (0, Infinity), NonDecreasing 1, NonIncreasing 2, NonDecreasing 3, NonIncreasing 4]
domains = Just [(1 ,  5),(1 ,  5),(1 ,  5),(1 ,  5),(1 ,  5)]
varnames = ["omega","T","h","kb","c"]
'''
constrs['I_48_20'] = '''shapes = [Image (0, Infinity), NonDecreasing 0, NonDecreasing 1, NonDecreasing 2]
domains = Just [(1 ,  5),(1 ,  2),(3 ,  20)]
varnames = ["m","v","c"]
'''
constrs['II_6_15a'] = '''shapes = [Image (0, 1e6), NonIncreasing 0, NonDecreasing 1, NonIncreasing 2, NonDecreasing 3, NonDecreasing 4, NonDecreasing 5]
domains = Just [(1 ,  3),(1 ,  3),(1 ,  3),(1 ,  3),(1 ,  3),(1 ,  3)]
varnames = ["epsilon","p_d","r","x","y","z"]
'''
constrs['II_11_27'] = '''shapes = [Image (0, 1e6), NonDecreasing 0, NonDecreasing 1, NonDecreasing 2, NonDecreasing 3]
domains = Just [(0 ,  1),(0 ,  1),(1 ,  2),(1 ,  2)]
varnames = ["n","alpha","epsilon","Ef"]
'''
constrs['II_11_28'] = '''shapes = [Image (0, 1e6), NonDecreasing 0, NonDecreasing 1]
domains = Just [(0 ,  1),(0 ,  1)]
varnames = ["n","alpha"]
'''
constrs['II_35_21'] = '''shapes = [Image (0, 1e6), NonDecreasing 0, NonDecreasing 1, NonDecreasing 2, NonIncreasing 3, NonIncreasing 4]
domains = Just [(1 ,  5),(1 ,  5),(1 ,  5),(1 ,  5),(1 ,  5)]
varnames = ["n_rho","mom","B","kb","T"]
'''
constrs['III_9_52'] = '''shapes = [Image (0, 1e6), NonDecreasing 0, NonDecreasing 1, NonIncreasing 3]
domains = Just [(1 ,  3),(1 ,  3),(1 ,  3),(1 ,  3),(1 ,  5),(1 ,  5)]
varnames = ["p_d","Ef","t","h","omega","omega_0"]
'''
constrs['III_10_19'] = '''shapes = [Image (0, Infinity), NonDecreasing 0, NonDecreasing 1, NonDecreasing 2, NonDecreasing 3]
domains = Just [(1 ,  5),(1 ,  5),(1 ,  5),(1 ,  5)]
varnames = ["mom","Bx","By","Bz"]
'''
constrs['Jackson_211'] = '''shapes = [Image (0, Infinity), NonDecreasing 0, NonIncreasing 1, NonDecreasing 2, NonDecreasing 3, NonDecreasing 4]
domains = Just [(1 ,  5),(1 ,  3),(1 ,  5),(4 ,  6),(1 ,  5)]
varnames = ["q","y","Volt","d","epsilon"]
'''
constrs['WavePower'] = '''shapes = [Image (-1e8, 0), NonIncreasing 0, NonDecreasing 1, NonIncreasing 2, NonIncreasing 3, NonDecreasing 4]
domains = Just [(1 ,  2),(1 ,  2),(1 ,  5),(1 ,  5),(1 ,  2)]
varnames = ["G","c","m1","m2","r"]
'''
constrs['SpatialCoevolution'] = '''shapes = [Image (0, 2), PartialNonIncreasing 0 (-5, 0), PartialNonDecreasing 0 (0, 5), PartialNonIncreasing 1 (-5, 0), PartialNonDecreasing 1 (0, 5)]
domains = Just [(-5, 5),(-5 , 5)]
varnames = ["x","y"]
'''
constrs['Vladislavleva-1'] = '''shapes = [Image (0, 1), PartialNonDecreasing 0 (-0.2, 1), PartialNonIncreasing 0 (1, 4.2), PartialNonDecreasing 1 (-0.2, 2.5), PartialNonIncreasing 1 (2.5, 4.2)]
domains = Just [(-0.2, 4.2),(-0.2 , 4.2)]
varnames = ["x1","x2"]
'''
constrs['Vladislavleva-4'] = '''shapes = [Image (0, 2), PartialNonDecreasing 0 (-0.25, 3), PartialNonIncreasing 0 (3, 6.35), PartialNonDecreasing 1 (-0.25, 3), PartialNonIncreasing 1 (3, 6.35), PartialNonDecreasing 2 (-0.25, 3), PartialNonIncreasing 2 (3, 6.35), PartialNonDecreasing 3 (-0.25, 3), PartialNonIncreasing 3 (3, 6.35), PartialNonDecreasing 4 (-0.25, 3), PartialNonIncreasing 4 (3, 6.35)]
domains = Just [(-0.25, 6.35),(-0.25, 6.35),(-0.25, 6.35),(-0.25, 6.35),(-0.25, 6.35)]
varnames = ["x1","x2","x3","x4","x5"]
'''

for fname, constr in constrs.items():
    for noise in noises:
        for alg in algs:
            fname_train = f"{fname}_{noise}_train.csv"
            fname_test = f"{fname}_{noise}_test.csv"
            npop = 500 if alg == "ITEA" else 250

            data = f"""[IO]
train = datasets/extrapolation/{fname_train}
test  = datasets/extrapolation/{fname_test}
task  = Regression
log   = PartialLog "results/extrapolation/{fname}_{noise}"

[Mutation]
exponents      = (-3, 3)
termlimit      = (2,15)
nonzeroexps    = 1
transfunctions = [Id, Sin, Cos, Tanh, SqrtAbs, Log, Exp]
measures       = ["NMSE", "RMSE", "MAE", "R^2"]

[Algorithm]
npop      = {npop}
ngens     = 1000
algorithm = {alg}

[Constraints]
penalty = NoPenalty
{constr}
"""
            fw = open(f"configs/{fname}_{noise}_{alg}.cfg", "w")
            fw.write(data)
            fw.close()

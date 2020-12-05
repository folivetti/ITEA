from funs import *

datanames = ["aircraft_lift", "flow_psi", "flow_stress",
                "I_15_3t", "I_15_3x", "I_26_2",
                "I_29_16", "I_30_5", "I_32_17",
                "I_37_4", "I_41_16", "I_48_20",
                "I_6_20", "I_6_20b", "I_8_14",
                "I_9_18", "II_11_27", "II_11_28",
                "II_35_21", "II_6_15a", "III_10_19",
                "III_9_52", "Jackson211", "rocket_fuel", "WavePower"]

def constraints(dataname, samples, expr, ws, b):
    
    Infinity = np.inf

    with open(f"../configs/noise/{dataname}_0.cfg") as f:
        lines = f.readlines()
        for l in lines:
            if l.startswith("codomain"):
                lb, ub = eval(l.split("=")[1])
            elif l.startswith("diffcodomains"):
                diffs = eval(l.split("=")[1])

        ys = evalExpr(expr, samples, ws, b)
        if not np.isinf(lb) and ys.min() < lb:
            return 1
        if not np.isinf(ub) and ys.max() > ub:
            return 1 

        for i, ds in enumerate(diffs):
            ys = evalDiff(expr, samples, ws, i)
            if ys is None:
                return 0
            if not np.isinf(ds[0]) and ys.min() < ds[0]:
                return 1
            if not np.isinf(ds[1]) and ys.max() > ds[1]:
                return 1
        return 0

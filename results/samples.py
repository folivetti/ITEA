import numpy as np

def samplesFromDomains(n, domains):
    dim = len(domains)
    samples = np.zeros((n,dim))
    for i, d in enumerate(domains):
        samples[:,i] = np.random.uniform(*d, n)
    return samples


def genSamples(dataname, n):
    with open(f"../configs/noise/{dataname}_0.cfg") as f:
        lines = f.readlines()
        for l in lines:
            if l.startswith("domains"):
                domains = eval(l.split("=")[1])
                samples = samplesFromDomains(n,domains)
                break
    return samples

import pandas as pd
import numpy as np

datasets = ["airfoil", "concrete", "energyCooling", "energyHeating", "towerData", "wineRed", "wineWhite", "yacht"]
datasets = ["airfoil", "concrete", "energyCooling", "energyHeating", "wineRed", "yacht"]
datasets = ["wineWhite", "towerData", "tecator", "Geographical"]

print(datasets)

for dname in datasets:
    df = pd.read_csv(f"{dname}/stats.csv")
    mu = np.round(df.RMSE_train.mean(), 2)
    st = np.round(df.RMSE_train.std(), 2)
    print(f"{mu} \\pm {st}", end=" & ")
print("\n")


for dname in datasets:
    df = pd.read_csv(f"{dname}/stats.csv")
    mu = np.round(df.RMSE_test.mean(), 2)
    st = np.round(df.RMSE_test.std(), 2)
    print(f"{mu} \\pm {st}", end=" & ")
print("\n")

for dname in datasets:
    df = pd.read_csv(f"{dname}/stats.csv")
    mu = np.round(df.RMSE_test.min(), 2)
    print(f"{mu}", end=" & ")
print("\n")

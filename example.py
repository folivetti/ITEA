import numpy as np
import pyITEA as itea

Z = np.loadtxt("datasets/airfoil/airfoil-train-0.dat", delimiter=",")
clr = itea.ITEARegressor(500,1000,(-5,5),(2,15),10)
clr.fit(Z[:,:-1], Z[:,-1])
yhat = clr.predict(Z[:,:-1])

mse = np.square(yhat - Z[:,-1]).mean()
print("Fitness should be approx.: ", np.sqrt(mse))
print(clr.expr)
print(clr.sympy)
print(clr.len)

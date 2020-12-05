import numpy as np
import itea

Z = np.loadtxt("datasets/airfoil/airfoil-test-0.dat", delimiter=",")
clr = itea.ITEARegressor(100,100,(-2,2),(2,15),10)
clr.fit(Z[:,:-1], Z[:,-1])
yhat = clr.predict(Z[:,:-1])

mse = np.square(yhat - Z[:,-1]).mean()
print("Fitness should be approx.: ", np.sqrt(mse))

import numpy as np
from itea.regression import ITEA_regressor
from itea.inspection import *
import warnings; warnings.filterwarnings('ignore')

tfuncs = {
    'log'      : np.log,
    'sqrt.abs' : lambda x: np.sqrt(np.abs(x)),
    'id'       : lambda x: x,
    'sin'      : np.sin,
    'cos'      : np.cos,
    'exp'      : np.exp
}

tfuncs_dx = {
    'log'      : lambda x: 1/x,
    'sqrt.abs' : lambda x: x/( 2*(np.abs(x)**(3/2)) ),
    'id'       : lambda x: np.ones_like(x),
    'sin'      : np.cos,
    'cos'      : lambda x: -np.sin(x),
    'exp'      : np.exp,
}

Z = np.loadtxt("datasets/airfoil/airfoil-train-0.dat", delimiter=",")
X = Z[:,:-1]
y = Z[:,-1]

clr = ITEA_regressor(gens=1000, popsize=100, expolim=(-3,3), max_terms=15, tfuncs=tfuncs, tfuncs_dx=tfuncs_dx)
clr.fit(X,y)
yhat = clr.predict(X)
print(np.square(y-yhat).mean())

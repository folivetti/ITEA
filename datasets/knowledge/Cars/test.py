import numpy as np
from numpy import sin, cos, sqrt, abs

def f(x):
    v = x[:,0]
    a = x[:,4]
    x0 = x[:,0]
    x4 = x[:,4]
    return 7.55581191044375 + 7.192879229027095*(np.cos(v**(3))) + -1.0291488902137037e-4*(np.sqrt(np.abs(v**(4)*a**(4)))) + -2.3704001662140276e-7    *((v*a**(4))) + 7.555811909623159*(np.cos(v**(-2)*a**(-3))) + 7.555811911795386*(np.cos(v**(-4)*a**(-2))) + 3.8992541640475954e-3*((v**(4)*a**(-1))) + -1.9603098905409944*(np.sin(v**(2)))
    
def nmse(y_real, y_pred):
    mse = np.square(y_real - y_pred).mean()
    return mse/y_real.var()
        
X_train = np.loadtxt("cars_train.csv", delimiter=",")
X_test  = np.loadtxt("cars_test.csv", delimiter=",")

y_pred = f(X_train)
y_real = X_train[:,-1]

print(nmse(y_real, y_pred))

y_pred = f(X_test)
y_real = X_test[:,-1]

print(nmse(y_real, y_pred))


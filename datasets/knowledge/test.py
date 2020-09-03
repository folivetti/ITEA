import numpy as np
from numpy import sin, cos, sqrt, abs, log, exp

def f(x):
    x0 = x[:,0]
    x1 = x[:,1]
    x2 = x[:,2]
    x3 = x[:,3]
    x4 = x[:,4]
    return -1.691309794799524e-8*exp(x0*x1**(-1)*x2**(2)*x4**(-4)) + 10.908017565069464*sqrt(x1**(4)*x4**(4)) + 75.2457371780136*log(x0**(-4)*x1**(4)*x2**(-4)*x3**(-4)*x4**(4)) -5.482860324996247*sqrt(x0**(4)*x1**(-4)*x2**(4)*x3**(4)*x4**(-4)) + 80.06122023311369*sqrt(abs(x0**(2)*x1**(-4)*x2**(3)*x3**(3)*x4**(-4))) + 154.1727661912772*sqrt(abs(x0**(4)*x1**(-4)*x2**(2)*x3**(2)*x4**(-4))) -71.66008750792857*(x0**(3)*x1**(-4)*x2**(2)*x3**(2)*x4**(-4))-91.55058013240578

def nmse(y_real, y_pred):
    mse = np.square(y_real - y_pred).mean()
    return mse/y_real.var()
        
X_train = np.loadtxt("GravitationalWavePower_train.csv", delimiter=",")
X_test  = np.loadtxt("GravitationalWavePower_test.csv", delimiter=",")

y_pred = f(X_train)
y_real = X_train[:,-1]

print(nmse(y_real, y_pred))

y_pred = f(X_test)
y_real = X_test[:,-1]

print(nmse(y_real, y_pred))


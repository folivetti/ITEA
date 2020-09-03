import numpy as np
from scipy.linalg import lstsq
from sklearn import linear_model

z = np.loadtxt("Aircraft_train.csv", delimiter=",")
x = z[:, :-1]
y = z[:, -1]

z = np.loadtxt("z.txt", delimiter=",")

it = np.zeros((x.shape[0],4))
for i in range(x.shape[0]):
    it[i,0] = np.prod(np.power(x[i,:], [1,0,0,0,0,0]))
    it[i,1] = np.prod(np.power(x[i,:], [1,1,0,0,0,0]))
    it[i,2] = np.prod(np.power(x[i,:], [0,0,1,1,1,-1]))
    it[i,3] = 1

reg = linear_model.LinearRegression()
reg.fit(it,y)

w1, _, _, _ = lstsq(it,y,lapack_driver='gelss')
w2 = reg.coef_

for wi, wj in zip(w1, w2):
    print(wi, wj)


reg.fit(z,y)

w1, _, _, _ = lstsq(z,y,lapack_driver='gelss')
w2 = reg.coef_

for wi, wj in zip(w1, w2):
    print(wi, wj)

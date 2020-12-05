from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.utils.validation import check_X_y, check_array, check_is_fitted

import os
import tempfile
import subprocess
import pandas as pd
import numpy as np
import re

def sqrtabs(a):
    """ Auxiliary function to calculate sqrt.abs """
    return np.sqrt(np.abs(a))

def ident(x):
    """ Identity function """
    return x


class ITEARegressor(BaseEstimator, RegressorMixin):

    def __init__(self, npop, ngens, exponents, termlimit, nonzeroexps=10, transfunctions='["id", "sin", "cos", "tanh", "sqrt.abs", "log", "exp"]'):
        """ Builds a Symbolic Regression model using ITEA.

        Parameters
        ----------
        npop : population size
        ngens : number of generations
        exponents : a tuple of int representing the minimum and maximum exponents of the interactions
        termlimit : a tuple of int representing the minimum and maximum number of terms in an expression
        nonzeroexps : number of non-zero exponents in each term of the initial population
        transfunctions : a string with the list of transformation functions enclosed by quotes. Currently supported: id, sin, cos, tan, tanh, sqrt, sqrt.abs, log, exp


        Examples
        --------
        >>> from itea import ITEARegressor
        >>> import numpy as np
        >>> X = np.arange(100).reshape(100, 1)
        >>> y = x**2
        >>> reg = SymbolicRegressor(100, 100, (1,2), (1,3))
        >>> reg.fit(X, y)
        """

        self.exponents = exponents
        self.termlimit = termlimit
        self.nonzeroexps = nonzeroexps
        self.transfunctions = transfunctions
        self.npop = npop
        self.ngens = ngens

        self.tmpdir = tempfile.mkdtemp()
        print(self.tmpdir)
    
    def insertNP(self, term):
        """ Prepend function names with 'np.' """
        idx = term.find("(")
        if term[idx+1:idx+8] != "sqrtabs" and term[idx+1:idx+6] != "ident":
            return term[:idx+1] + "np." + term[idx+1:]
        return term

    def fit(self, X_train, y_train):
        """A reference implementation of a fitting function.
        Parameters
        ----------
        X : {array-like, sparse matrix}, shape (n_samples, n_features)
            The training input samples.
        y : array-like, shape (n_samples,) or (n_samples, n_outputs)
            The target values (class labels in classification, real numbers in
            regression).
        Returns
        -------
        self : object
            Returns self.
        """

        X_train, y_train = check_X_y(X_train, y_train, accept_sparse=False)
        if len(y_train.shape) == 1:
            Z_train = np.hstack((X_train, y_train[:,None]))
        else:
            Z_train = np.hstack((X_train, y_train))
        
        fname   = self.tmpdir + "/tmpdata.csv"
        logname = self.tmpdir + "/tmp"
        cfgname = self.tmpdir + "/tmp.cfg"
        
        np.savetxt(f"{fname}", Z_train, delimiter=",")
        
        config = f'''[Dataset]
train = {fname}
test  = {fname}
task  = Regression

[Mutation]
exponents = {self.exponents}
termlimit = {self.termlimit}
nonzeroexps = {self.nonzeroexps}
transfunctions = {self.transfunctions}
measures       = ["RMSE", "NMSE", "MAE", "R^2"]

[Algorithm]
npop = {self.npop}
ngens = {self.ngens}
log = PartialLog "{logname}"
'''
        fw = open(f"{cfgname}", "w")
        fw.write(config)
        fw.close()

        subprocess.call([f"stack run config {cfgname}"], shell=True)

        df = pd.read_csv(f"{logname}/stats.csv")
        self.expr = df.expr.values[0]
        
        self.is_fitted_ = True

        return self

    def eval_expr(self, x):
        """ Evaluates the expression with data point x. """
        terms = (self.expr
                 .replace('id','ident')
                 .replace('sqrt.abs','sqrtabs')
                 .replace('^','**')
                 .split(' + '))
        bias  = terms[0]
        terms = terms[1:]
        p = re.compile('x([0-9]+)')
        terms = map(lambda t: self.insertNP(p.sub(r"x[:,\1]", t)), terms)

        zs = np.array(list(map(eval,terms))).T

        return zs.sum(axis=1) + eval(bias)

    def predict(self, X_test, ic=None):
        """ A reference implementation of a predicting function.
        Parameters
        ----------
        X : {array-like, sparse matrix}, shape (n_samples, n_features)
            The training input samples.
        Returns
        -------
        y : ndarray, shape (n_samples,)
            Returns an array of ones.
        """        
        check_is_fitted(self)
        X_test = check_array(X_test, accept_sparse=False)
        return self.eval_expr(X_test)

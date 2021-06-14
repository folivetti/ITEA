from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.utils.validation import check_X_y, check_array, check_is_fitted

import os
import tempfile
import subprocess
import pandas as pd
import numpy as np
import re

def sqrtAbs(a):
    """ Auxiliary function to calculate sqrt.abs """
    return np.sqrt(np.abs(a))


class ITEARegressor(BaseEstimator, RegressorMixin):

    def __init__(self, npop, ngens, exponents, termlimit, nonzeroexps=10, transfunctions='[Id, Sin, Tanh, SqrtAbs, Log, Exp]', random_state=None):
        """ Builds a Symbolic Regression model using ITEA.

        Parameters
        ----------
        npop : population size
        ngens : number of generations
        exponents : a tuple of int representing the minimum and maximum exponents of the interactions
        termlimit : a tuple of int representing the minimum and maximum number of terms in an expression
        nonzeroexps : number of non-zero exponents in each term of the initial population
        transfunctions : a string with the list of transformation functions. Currently supported: Id, Sin, Cos, Tanh, SqrtAbs, Log, Exp


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
        self.random_state = random_state
        #print(self.tmpdir)
    
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
        
        self.termlimit = (2, max(2, min(15, X_train.shape[0]//10)))
        
        np.savetxt(f"{fname}", Z_train, delimiter=",")
        
        config = f'''[IO]
train = {fname}
test  = {fname}
task  = Regression
log   = PartialLog "{logname}"

[Mutation]
exponents      = {self.exponents}
termlimit      = {self.termlimit}
nonzeroexps    = {self.nonzeroexps}
transfunctions = {self.transfunctions}
measures       = ["RMSE"]

[Algorithm]
npop      = {self.npop}
ngens     = {self.ngens}
algorithm = ITEA
seed = {"Just " + str(self.random_state) if self.random_state is not None else "Nothing"}

[Constraints]
penalty = NoPenalty
shapes  = []
domains = Nothing
varnames = []
'''
        fw = open(f"{cfgname}", "w")
        fw.write(config)
        fw.close()

        cwd = os.path.dirname(os.path.realpath(__file__))
        #print(cwd)
        #subprocess.call([f"stack run config {cfgname}"], shell=True, cwd=cwd)
        subprocess.call([f"bin/itea config {cfgname}"], shell=True, cwd=cwd)
        #print(ooo)

        df = pd.read_csv(f"{logname}/exprs.csv")
        self.expr = df.python.values[0]
        
        df = pd.read_csv(f"{logname}/stats.csv")
        self.len = df.length.values[0]
        
        self.is_fitted_ = True

        return self

    def eval_expr(self, x):
        """ Evaluates the expression with data point x. """
        Z = eval(self.expr)
        inds = np.where(np.isnan(Z))[0]
        inds2 = np.where(np.isinf(Z))[0]
        Z[inds] = 0
        Z[inds2] = 0
        #print(Z)

        return Z

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

import re
import numpy as np
from sklearn.linear_model import LinearRegression

def sqrtabs(a):
    return np.sqrt(np.abs(a))

def ident(x):
    return x

def insertNP(term):
    if term[:7] != "sqrtabs" and term[:5] != "ident":
        return "np." + term
    return term

def getWeights(expr, x, y):
    terms = expr.replace('id','ident').replace('sqrt.abs','sqrtabs').replace('^','**').split(' + ')
    p = re.compile('x([0-9])')
    terms = list(map(lambda t: insertNP(p.sub(r"x[:,\1]", t)), terms))
    zs = np.array(list(map(eval,terms))).T
    lr = LinearRegression()
    lr.fit(zs,y)
    return lr.coef_, lr.intercept_

def evalExpr(expr, x, ws, b):
    terms = expr.replace('id','ident').replace('sqrt.abs','sqrtabs').replace('^','**').split(' + ')
    p = re.compile('x([0-9])')
    terms = list(map(lambda t: insertNP(p.sub(r"x[:,\1]", t)), terms))
    zs = np.array(list(map(eval,terms))).T
    ys = (ws*zs).sum(axis=1) + b 
    return ys

def dtan(x):
    return np.cos(x)**(-2)
def dtanh(x):
    return 1 - np.tanh(x)**2
def dsqrt(x):
    return 2.0/np.sqrt(x)
def dsqrtabs(x):
    return x/(2*np.abs(x)**(1.5))
def dlog(x):
    return 1/x
def dident(x):
    return np.ones(np.shape(x))

def diff(t):
    if t.startswith("np.sin"):
        return "np.cos" + t[6:]
    elif t.startswith("np.cos"):
        return "-np.sin" + t[6:]
    elif t.startswith("np.tanh"):
        return "dtanh" + t[7:]
    elif t.startswith("np.tan"):
        return "dtan" + t[6:]
    elif t.startswith("np.sqrt"):
        return "dsqrt" + t[7:]
    elif t.startswith("sqrtabs"):
        return "dsqrtabs" + t[7:]
    elif t.startswith("np.log"):
        return "dlog" + t[6:]
    elif t.startswith("ident"):
        return "dident" + t[5:]
    else:
        return t
        
def getCoeff(t,i):
     pos = t.find(f"x[:,{i}]")
     ti = t[pos+6:]
     if ti.startswith("**"):
         pos2 = ti.find(")")
         return float(ti[3:pos2])
     return 1.0 

def evalDiff(expr, x, ws, i):
    terms = expr.replace('id','ident').replace('sqrt.abs','sqrtabs').replace('^','**')
    terms = terms.split(' + ')
    
    coeffs = np.array(list(map(lambda t: getCoeff(t,i), terms)))
    
    p1 = re.compile('x([0-9])')
    p2 = re.compile('(\(.+\))')
    terms = list(filter(lambda t: f'x[:,{i}]' in t, terms))
    if len(terms) == 0:
        return None
        
    terms = list(map(lambda t: insertNP(p1.sub(r"x[:,\1]", t)), terms))
    terms = list(map(lambda t: p2.sub(r"\1*\1/x[:,{}]".format(i), t), terms))
    terms = list(map(diff,terms))
    zs = np.array(list(map(eval,terms))).T
    ys = (coeffs*ws*zs).sum(axis=1)  
    return ys

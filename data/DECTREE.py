#http://stackoverflow.com/questions/25239958/impute-categorical-missing-values-in-scikit-learn
import pandas as pd
import numpy as np

from sklearn.base import BaseEstimator
from sklearn.base import ClassifierMixin
from sklearn.cross_validation import StratifiedKFold
from sklearn.grid_search import GridSearchCV
from sklearn.tree import DecisionTreeClassifier
class AutoTunedDecisionTreeClassifier(BaseEstimator, ClassifierMixin):

    def __init__(self):
	self._parametergrid= {"criterion" : ("gini", "entropy"),"max_depth":(1,2,3,4,5,6,7,8,9,10),"min_samples_leaf":(1,2,3,4,5,6,7,8,9,10)
             }
    def fit(self, X, y=None):      
	mycv = StratifiedKFold(y[y.columns[0]], n_folds = 2, shuffle=True,random_state=0)
	#refit=True refits the best estimator to the entire training dataset
	clf = GridSearchCV(DecisionTreeClassifier(random_state=0), self._parametergrid,refit=True,cv=mycv)
	clf.fit(X, y[y.columns[0]])
	self._best_estimator =  clf.best_estimator_
	print "Best parameters"
	print clf.best_params_
        return self._best_estimator
      
    def predict(self, X, check_input=True): 
      return self._best_estimator.predict(X,check_input)
    
    def apply(self, X, check_input=True):
      return self._best_estimator.apply(X,check_input)
         
    def predict_proba(self, X, check_input=True):
      return self._best_estimator.predict_proba(X, check_input)
             
    def predict_log_proba(self, X):
      return self._best_estimator.predict_log_proba(X)

    def score(self, X, y, sample_weight=None):
      return self._best_estimator.score(X, y, sample_weight)
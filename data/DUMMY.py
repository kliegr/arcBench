#http://stackoverflow.com/questions/25239958/impute-categorical-missing-values-in-scikit-learn
import pandas as pd
import numpy as np

from sklearn.base import TransformerMixin

class ConvertCategoricalToDummies(TransformerMixin):   
      
    def fit(self, X, y=None):
	self._fitted_cols=pd.get_dummies(X).columns
        return self

    def transform(self, X, y=None):       
        intResult = pd.get_dummies(X)  
        
        if len(intResult.columns)!=len(self._fitted_cols):
          #drop columns, which are not among the fitted        
          colsToRemove=set(intResult.columns)-set(self._fitted_cols)
          if len(colsToRemove)>0:
            intResult.drop(colsToRemove,axis=1,inplace=True)
          colsToAdd=set(self._fitted_cols)-set(intResult.columns)
          #add columns, which are among the fitted but not among the transformed
          for col in colsToAdd :
            intResult[col] = pd.Series(np.zeros(len(intResult)), index=intResult.index)	    
        return intResult
        


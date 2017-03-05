#http://stackoverflow.com/questions/25239958/impute-categorical-missing-values-in-scikit-learn
import pandas as pd
import numpy as np

from sklearn.base import TransformerMixin

class DataFrameImputer(TransformerMixin):

    def __init__(self,replacement_for_cat=None):
	self._replacement_for_cat=replacement_for_cat
        """Impute missing values.

        Columns of dtype object are imputed with the most frequent value 
        in column.

        Columns of other types are imputed with mean of column.

        """
    def fit(self, X, y=None):
        #
	#it is necessary to remove columns in fit, before the replacements are set.
	origCols=X.columns
	X.dropna(axis=1,how="all", inplace=True)   
	self.colsToKeep=set(X.columns)
	if  len(self.colsToKeep) < len(origCols):
	  print "removed na columns" + str(len(set(origCols)-set(self.colsToKeep)))
	
	if self._replacement_for_cat==False:	  
	  #do not replace missing categorical values 
	  self.fill = pd.Series([np.nan
            if X[c].dtype == np.dtype('O') else X[c].mean() for c in X], index=X.columns)
			  
	else:
	  #replace all missing categorical values with the most frequent value in the given column	  
	  self.fill = pd.Series([X[c].value_counts().index[0]
            if X[c].dtype == np.dtype('O') else X[c].mean() for c in X],
            index=X.columns)	

        return self

    def transform(self, X, y=None):
        colsToRemove=set(X.columns)-set(self.colsToKeep)
        if len(colsToRemove)>0:
            X.drop(colsToRemove,axis=1,inplace=True)
        return X.fillna(self.fill)

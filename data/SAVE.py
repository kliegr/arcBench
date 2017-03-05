import pandas as pd
import numpy as np
import MDLP as mdlp
from sklearn.base import TransformerMixin

class DiscretizeAndSaveToCSV(TransformerMixin):
    i=-1
    def __init__(self, dirToSave, baseFileName,discretize=True,save=True,outputDiscretizedData=True):     
      self._dirToSave=dirToSave
      self._baseFileName=baseFileName
      self._discretize=discretize
      self._save=save
      self._outputDiscretizedData = outputDiscretizedData
      global i
      i=-1

    def fit(self, X, y=None):	
	self._phase="train"
	global i
	i=i+1
        print "fold ", str(i)
        if self._save:
	  y.to_csv(self._dirToSave + "/"+ self._baseFileName + "_" + self._phase + "Y_" + str(i) + ".csv", index=False)
	if self._discretize:
	  completeDataset=pd.concat([X,y], axis=1,join_axes=[X.index])
	  self._discretizer = mdlp.MDLP_Discretizer(dataset=completeDataset, class_label=y.columns[0], features=None, out_path_data=None, out_path_bins=None)	
        return self

    def transform(self, X, y=None):    
        #global i        
        if self._discretize:
            if self._save:
                X.to_csv(self._dirToSave + "/"+ self._baseFileName + "_" + self._phase + "X_BEFORE_DISCR" + str(i) + ".csv", index=False)                
            self._discretizer.applyOnData(dataset=X, out_path_data=None)
            Xdisc=self._discretizer.discretizedData        
        if self._discretize and self._outputDiscretizedData:
            out = Xdisc
        else:
            out = X
        if self._save:
            out.to_csv(self._dirToSave + "/"+ self._baseFileName + "_" + self._phase + "X_" + str(i) + ".csv", index=False)        
        self._phase="test"
        return out

        


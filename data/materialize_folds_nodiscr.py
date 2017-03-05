from sklearn.cross_validation import StratifiedKFold,cross_val_score
import pandas as pd
import os
from sklearn import svm
from sklearn import preprocessing
from sklearn.preprocessing import OneHotEncoder
from sklearn.tree import DecisionTreeClassifier
from sklearn.feature_extraction import DictVectorizer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import Imputer
from sklearn.base import TransformerMixin
import MDLP as mdlp
import DUMMY as dummy
import IMPUTE as imp
import DECTREE as dec
import SAVE as sav
  
from datasets import *

if not os.path.exists("output"):
    os.makedirs("output")
if not os.path.exists("folds_nodiscr"):
    os.makedirs("folds_nodiscr")
if not os.path.exists("folds_nodiscr/test"):
    os.makedirs("folds_nodiscr/test")
if not os.path.exists("folds_nodiscr/train"):
    os.makedirs("folds_nodiscr/train")
#uses a specific classifier, but the actual results of classification are discarded
classifiers=[{"instance":DecisionTreeClassifier(random_state=0,max_depth=1,min_samples_split=100), "optimizedparameters":False, "requireDiscretized":False, "file":"results-pydectree-prediscretized"}]


for classifier in classifiers: 
  for dataset in datasets:  
    print "processing: "+dataset["filename"]
    dfY=pd.read_csv("datasets/"+dataset["filename"]+".csv",delimiter=",",index_col=False, usecols=[dataset["targetvariablename"]])
    dfX=pd.read_csv("datasets/"+dataset["filename"]+".csv",delimiter=",",index_col=False).drop(dataset["targetvariablename"],1)  
    if 'id' in dataset:
      dfX.drop(dataset["id"],axis=1,inplace=True)
    dfX=dfX.dropna(axis=1,how="all")
    if  dataset["filename"] == "anneal":
        random_state=1 #for random state 0 the fold generation crashes for some unknown reason
    else:
        random_state=0
    skf = StratifiedKFold(dfY[dataset["targetvariablename"]], n_folds=10,shuffle=True,random_state=random_state) #
    clf = Pipeline([('missingVals', imp.DataFrameImputer(replacement_for_cat=False)), ("discretize",sav.DiscretizeAndSaveToCSV(dirToSave="output",baseFileName=dataset["filename"],discretize=dataset["numerical"] if classifier["requireDiscretized"] else False,save=True,outputDiscretizedData=True)), ('dummyConv', dummy.ConvertCategoricalToDummies()),('autdectree', classifier["instance"])])   
    scores = cross_val_score(clf,X=dfX, y=dfY, cv=skf)
    i=0
    for train_index, test_index in skf:    
       Y_test = dfY.iloc[test_index]    
       Y_test.to_csv("output/"+dataset["filename"]+"_testY_"+str(i)+".csv", index=False)      
       i=i+1


for dataset in datasets:
    for i in range(0,10):        
        trainX = "output/"+dataset["filename"]+"_trainX_" + str(i)+".csv"
        trainY = "output/"+dataset["filename"]+"_trainY_" + str(i)+".csv"
        trainOut = "folds_nodiscr/train/"+dataset["filename"]+ str(i)+".csv"
        
        testX = "output/"+dataset["filename"]+"_testX_" + str(i)+".csv"
        testY = "output/"+dataset["filename"]+"_testY_" + str(i)+".csv"
        testOut = "folds_nodiscr/test/"+dataset["filename"] + str(i)+".csv"
        
        trainX_df = pd.read_csv(trainX, delimiter=",")      
        trainY_df = pd.read_csv(trainY, delimiter=",")      
        train= pd.concat([trainX_df,trainY_df],axis=1)  

        testX_df = pd.read_csv(testX, delimiter=",")      
        testY_df = pd.read_csv(testY, delimiter=",")              
        test= pd.concat([testX_df,testY_df],axis=1)  

        train.to_csv(trainOut,index=False)
        test.to_csv(testOut,index=False)


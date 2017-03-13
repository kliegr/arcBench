from sklearn.cross_validation import StratifiedKFold,cross_val_score
import pandas as pd
from sklearn.tree import DecisionTreeClassifier

from datasets import *

for dataset in datasets:
    df=pd.read_csv("datasets/"+dataset["filename"]+".csv",delimiter=",",index_col=False)
    missing = "N"
    if dataset["numerical"]==False:
        continue;
    if len(df[df.isnull().any(axis=1)]):
        missing = "Y"
    print dataset["filename"] + " & " + str(len(df.columns)) + " & " + str(len(df)) +  " & " + missing

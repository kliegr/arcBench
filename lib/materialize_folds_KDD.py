from sklearn.datasets import fetch_kddcup99
from sklearn.model_selection import StratifiedKFold
import pandas as pd
try:
    kdd=pd.read_csv("datasets/kddfull.csv")
except FileNotFoundError:
    print("fetching")
    kdd = fetch_kddcup99(as_frame=True).frame
    kdd.to_csv("data/datasets/kddfull.csv")

for i in [1000,10000,20000,30000,40000,50000]:
    df=kdd.sample(n=i)
    df.to_csv("data/datasets/kdd"+str(i)+"_.csv")
    skf=StratifiedKFold(n_splits=10, random_state=42, shuffle=True)
    X=df.drop('labels', axis=1)
    for column in df:
        if df.dtypes[column] != "O":
            X[column]=X[column].str.decode('UTF-8')
    y=df.labels.str.decode('UTF-8')
    splitID=0
    for train_index, test_index in skf.split(X, y):
        print("processing subsample of:", i, " rows, split", splitID)
        X_train, X_test = X.iloc[train_index,:] , X.iloc[test_index,:]
        y_train, y_test = y.iloc[train_index], y.iloc[test_index]
        pd.concat([X_train,y_train],axis=1).to_csv("data/folds_nodiscr/train/kdd" + str(i) + str(splitID)+ ".csv")
        pd.concat([X_test,y_test],axis=1).to_csv("data/folds_nodiscr/test/kdd" + str(i) + str(splitID) + ".csv")
        splitID = splitID+1
    

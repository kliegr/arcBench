import pandas as pd 

datasets = ["iris","australian","anneal","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","ionosphere","labor","letter","lymph","segment","sonar","vehicle","vowel","hepatitis","spambase","hypothyroid","kdd1000_","kdd10000_","kdd20000_","kdd30000_","kdd40000_","kdd50000_","kddfull"]    
print("Dataset,total unique vals,unique for numerical attributes")
for dataset in datasets:
    #print("Processing dataset " + dataset)
    df = pd.read_csv("data/datasets/{}.csv".format(dataset))
    ds_numeric = df.select_dtypes(include='number')
    #print(ds_numeric.nunique())
    print(dataset + "," + str(sum(df.nunique())) + "," + str(sum(ds_numeric.nunique())))

print("discretized datasets (fold 0)")
for dataset in datasets:
    #print("Processing dataset " + dataset)
    df = pd.read_csv("data/folds_discr2/train/{}0.csv".format(dataset))
    ds_numeric = df.select_dtypes(include='number')
    #print(ds_numeric.nunique())
    print(dataset + "," + str(sum(df.nunique())) + "," + str(sum(ds_numeric.nunique())))

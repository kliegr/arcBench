import os
import time
from sklearn.preprocessing import LabelEncoder
from corels import *
import pandas as pd
import math as math
basepath = "./"
datasets = ["hepatitis","ionosphere","sonar","spambase","australian", "breast-w", "colic", "credit-a",  "diabetes", "heart-statlog","credit-g","kdd1000_","kdd10000_","kdd20000_","kdd30000_","kdd40000_","kdd50000_"]
modelFolder = "CORELS_models"
resultFolder = "CORELS_results"
saveRules = True
separateFilePerDataset=False

def run1fold(datasetname):
    df_stat = pd.DataFrame(columns=['corels', 'corelsqcba'], index=["accuracy", "rulecount", "rulelength", "buildtime"])
    data_train_disc = pd.read_csv(basepath + "data/folds_discr2/train/{}.csv".format(datasetname))
    data_test_disc = pd.read_csv(basepath + "data/folds_discr2/test/{}.csv".format(datasetname))
    df_merged = data_train_disc.append(data_test_disc)
    # conversion to str is required otherwise getdummies skips numerical columns
    df_merged = pd.get_dummies(df_merged.astype(str))
    df_merged = df_merged.drop(df_merged.columns[-1], axis=1)
    dumFilePath=basepath +"data/folds_discr_dumm/train/{}.csv".format(datasetname)
    df_merged.head(len(data_train_disc.index)).to_csv(dumFilePath, index=False)
    dumFilePath_test = basepath + "data/folds_discr_dumm/test/{}.csv".format(datasetname)
    df_merged.tail(len(data_test_disc.index)).to_csv(dumFilePath_test, index=False)
    X, y, featurenames, classname = load_from_csv(dumFilePath)
    c = CorelsClassifier(n_iter=10000)
    start = time.time()
    c.fit(X, y, features=featurenames)
    end = time.time()
    df_stat.loc["buildtime", "corels"] = end - start
    X_test, y_test, featurenames_test, classname_test = load_from_csv(dumFilePath_test)
    df_stat.loc["accuracy", "corels"] = c.score(X_test,y_test)
    df_stat.loc["rulecount", "corels"] = len(c.rl().rules)
    antLengths = list(map(lambda r: len(r["antecedents"]), c.rl().rules))
    df_stat.loc["rulelength", "corels"] = sum(antLengths) / len(antLengths)
    print("Rule Count CORELS:", df_stat.loc["rulecount", "corels"])
    if (saveRules):
        RulesPath = basepath + modelFolder + "/{}.csv".format(datasetname)
        # c.rl().save(RulesPath) does not give nicely formatted result
        rules = c.rl()
        file = open(RulesPath, "w")
        file.write(str(rules))
        file.close()
    return df_stat

def mean_allfolds(dataset_name, start=0, end=10):
    files = [ dataset_name + repr(i) for i in range(start, end) ]
    df_agg = None
    emptyDF = True
    for file in files:
        df_stats = run1fold(file)
        print("done", file)
        if emptyDF:
            df_agg = df_stats
            emptyDF = False
        else:
            df_agg = df_agg + df_stats
        print(df_agg)
    foldcount = end-start
    print(foldcount)
    print(df_agg)
    df_agg= df_agg / foldcount
    print(df_agg)
    return df_agg

if __name__ == '__main__':
    print("Postprocessing by QCBA not yet supported")
    if not os.path.exists(basepath + modelFolder):
        os.makedirs(basepath + modelFolder)
    if saveRules and not os.path.exists(basepath +resultFolder):
        os.makedirs(basepath +resultFolder)
    for dataset in datasets:
        print(dataset)
        if separateFilePerDataset:
            resultFile = basepath + resultFolder + "/" + dataset + ".csv"
            if os.path.exists(basepath + resultFile):
                print("skipping already computed")
                continue
        else:
            resultFileCORELS = basepath + resultFolder + "/CORELS.csv"
            # TODO: refactor this into functions
            if not os.path.exists(resultFileCORELS):
                print("initializing result file")
                file = open(resultFileCORELS, "w+")
                file.write("dataset,accuracy,rules,antlength,buildtime\n")
                file.close()
            else:
                file = open(resultFileCORELS, "r+")
                strings = file.read()
                file.close()
                if "\n" + dataset + "," in strings:
                    print("skipping CORELS already computed")
                    continue
                    # Compute

        df_stats_per_dataset = mean_allfolds(dataset, start=0, end=10)
        print("*****")
        print(df_stats_per_dataset)
        print("******")

        # Save
        if separateFilePerDataset:
            df_stats_per_dataset.to_csv(resultFile)
        else:
            df_stats_per_dataset
            file = open(resultFileCORELS, "a")
            file.write(dataset + "," + str(df_stats_per_dataset.loc["accuracy", "corels"]) + "," + str(
                df_stats_per_dataset.loc["rulecount", "corels"]) + "," + str(
                df_stats_per_dataset.loc["rulelength", "corels"]) + "," + str(df_stats_per_dataset.loc["buildtime", "corels"]) + "\n")
            file.close()

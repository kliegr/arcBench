# Jiri Filip, Tomas Kliegr, GNU GPL 3

import pandas as pd
import pyarc
from pyids import IDS
from pyids.ids_classifier import mine_CARs
import time
import os

from pyarc.qcba.transformation import QCBATransformation

from pyarc import CBA, TransactionDB
from pyarc.qcba.data_structures import QuantitativeDataFrame
import pandas as pd

from pyarc.qcba.data_structures import (
    IntervalReader,
    Interval,
    QuantitativeDataFrame,
    QuantitativeCAR
)

from pyarc.qcba.classifier import QuantitativeClassifier
from sklearn.metrics import accuracy_score

interval_reader = IntervalReader()

interval_reader.closed_bracket = "", "NULL"
interval_reader.open_bracket = "NULL", ""
interval_reader.infinity_symbol = "inf", "inf"
interval_reader.members_separator = "_to_"

interval_reader.compile_reader()

QuantitativeCAR.interval_reader = interval_reader

basepath="/home/tomas/temp/arcBench/"
unique_transactions= True

def run1fold(basepath,datasetname, unique_transactions=True):
    df_stat = pd.DataFrame(columns=['ids','idsqcba'], index=["accuracy","rulecount","rulelength","buildtime"])
    data_train_disc = pd.read_csv(basepath+"data/folds_discr/train/{}.csv".format(datasetname))        
    data_train_undisc = pd.read_csv(basepath+"data/folds/train/{}.csv".format(datasetname))
    data_test_disc = pd.read_csv(basepath+"data/folds_discr/test/{}.csv".format(datasetname))
    data_test_undisc = pd.read_csv(basepath+"data/folds/test/{}.csv".format(datasetname))
    
    
    quant_dataframe_train_disc = QuantitativeDataFrame(data_train_disc)
    quant_dataframe_train_undisc = QuantitativeDataFrame(data_train_undisc)
    quant_dataframe_test_disc = QuantitativeDataFrame(data_test_disc)
    quant_dataframe_test_undisc = QuantitativeDataFrame(data_test_undisc)
    
    actual = quant_dataframe_test_undisc.dataframe.iloc[:,-1].values
    
    #learn rules for IDS
    cars = mine_CARs(data_train_disc, 50, sample=True)
    #train IDS model
    ids = IDS()
    start = time.time()
    ids.fit(class_association_rules=cars, quant_dataframe=quant_dataframe_train_disc, debug=False)
    end = time.time()
    df_stat.loc["buildtime","ids"] = end-start
    #apply IDS model
    # ?? bylo quant_dataframe_train_disc
    df_stat.loc["accuracy","ids"] =  ids.score(quant_dataframe_test_disc)    
    print("Acc IDS:",df_stat.loc["accuracy","ids"]  )
    df_stat.loc["rulecount","ids"] = len(ids.clf.rules)    
    antLengths=list(map(lambda r: len(r.car.antecedent.itemset.items()),ids.clf.rules ))
    df_stat.loc["rulelength","ids"]=sum(antLengths)/len(antLengths)
    
    avg_rule_legnth_ids = None
    print("Rule Count IDS:", df_stat.loc["rulecount","ids"])
    
    #postprocess IDS model with QCBA
    rules_to_optimize = ids.clf.rules
    start = time.time()
    quant_rules = [ QuantitativeCAR(r.car) for r in rules_to_optimize ]
    qcba_transformation = QCBATransformation(quant_dataframe_train_undisc)
    transformed_rules = qcba_transformation.transform(quant_rules)
    end = time.time()
    df_stat.loc["buildtime","idsqcba"] = end-start
    rules, default_class = transformed_rules    
    antLengths=list(map(lambda r: len(r.car.antecedent.itemset.items()),ids.clf.rules ))
    #+1 because the default rule is not counted
    df_stat.loc["rulelength","idsqcba"]=sum(antLengths)/(len(antLengths)+1)

    #apply QCBA model        
    qclf = QuantitativeClassifier(rules, default_class)
    # ?? Lze předat undisc data? bylo quant_dataframe_train_disc
    pred = qclf.predict(quant_dataframe_test_undisc) 
    
    #evaluate model - QCBA
    df_stat.loc["accuracy","idsqcba"] =  accuracy_score(actual, pred)    
    df_stat.loc["rulecount","idsqcba"] = len(rules)
    print("Acc IDS-QCBA:", df_stat.loc["accuracy","idsqcba"] )
    print("Rule Count IDS-QCBA:", df_stat.loc["rulecount","idsqcba"])
    return df_stat

def mean_allfolds(dataset_name, start=0, end=10, unique_transactions=True):
    files = [ dataset_name + repr(i) for i in range(start, end) ]
    df_agg = None
    emptyDF = True
    for file in files:
        df_stats = run1fold(basepath,file, unique_transactions=unique_transactions)
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

datasets = [
    "iris",
    "breast-w",
    "vehicle",
    "glass",
    "heart-h",
    "tic-tac-toe",
    "australian",
    "sick",
    "segment",
    "spambase",
    "sonar",
    "vowel",
    "hepatitis",
    "credit-a",
    "mushroom",
    "house-votes-84",
    "soybean",
    "primary-tumor",
    "credit-g",
    "audiology",
    "breast-cancer",
    "balance-scale",
    "heart-c",
    "kr-vs-kp",
    "pima",
    "heart-statlog",
    "diabetes",
    "hypothyroid", # slow extend
    "autos", # slow extend
    "lymph", # slow extend
    "anneal", #slow extend
    "ionosphere", #ValueError: min() arg is an empty sequence
    ]    

resultFolder="ids"
if not os.path.exists(resultFolder):
    os.makedirs(resultFolder)
for dataset in datasets:
    print(dataset)
    resultFile=resultFolder+"/"+dataset+"-result.csv"
    if os.path.exists(resultFile):
        print("skipping already computed")
        continue
    df_stats_per_dataset = mean_allfolds(dataset)    
    df_stats_per_dataset.to_csv(resultFile)
    print("*****")
    print(df_stats_per_dataset)
    print("******")


# Jiri Filip, Tomas Kliegr, GNU GPL 3

import pandas as pd
import pyarc
from pyids import IDS
from pyids.data_structures.ids_classifier import mine_CARs
from pyids.rule_mining import RuleMiner
from pyids.model_selection import mode
import time
import os

from pyarc.qcba.transformation import QCBATransformation

from pyarc.data_structures import TransactionDB
from pyarc import CBA
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

basepath="./"
unique_transactions= True

def run1fold(basepath,datasetname, unique_transactions=True,runQCBA=False,saveIDSRules=True,useConfidenceForCandidateGeneration=True):
    df_stat = pd.DataFrame(columns=['ids','idsqcba'], index=["accuracy","rulecount","rulelength","buildtime"])
    if (runQCBA):        
        #python QCBA implementation uses custom discretization format
        data_train_disc = pd.read_csv(basepath+"data/folds_discr/train/{}.csv".format(datasetname))        
        data_test_disc = pd.read_csv(basepath+"data/folds_discr/test/{}.csv".format(datasetname))
        data_test_undisc = pd.read_csv(basepath+"data/folds_nodiscr/test/{}.csv".format(datasetname))
        data_train_undisc = pd.read_csv(basepath+"data/folds_nodiscr/train/{}.csv".format(datasetname))
        
        quant_dataframe_test_undisc = QuantitativeDataFrame(data_test_undisc)
        quant_dataframe_train_undisc = QuantitativeDataFrame(data_train_undisc)
    else:
        #R QCBA implementation uses different discretization format, folds are generated with preprocess_for_ids.R
        data_train_disc = pd.read_csv(basepath+"data/folds_discr2/train/{}.csv".format(datasetname))        
        data_test_disc = pd.read_csv(basepath+"data/folds_discr2/test/{}.csv".format(datasetname))

    
    
    quant_dataframe_train_disc = QuantitativeDataFrame(data_train_disc)
    quant_dataframe_test_disc = QuantitativeDataFrame(data_test_disc)
    
    actual = quant_dataframe_test_disc.dataframe.iloc[:,-1].values
    

    if useConfidenceForCandidateGeneration:
        # mine_CARs learns initial candidate rules with CBA-like approach
        # it uses unsupervised paramter tuning to determine conf, supp and len thresholds, 
        # as described in Kliegr & Kuchar, 2019
        # Because the subsequent optimization is slow, not all initial candidate rules can be passed to IDS.
        # the sample parameter controls, how the subset of N rules will be selected from the initial candidates:
        # sample=False: take top N rules according to CBA criteria. According to our experiments, this has better results
        # sample=True: take random N rules

        cars = mine_CARs(data_train_disc, 50, sample=False)
    else:
        # learn candidate rules using approach without min confidence described in Lakkaraju et al, 2-16
        print("WARNING save any unsaved work")
        print("WARNING candidate generation without minimum confidence and sampling may be too slow or memory intensive")
        rm = RuleMiner()
        cars = rm.mine_rules(data_train_disc, minsup=0.01) # the 0.01 threshold is from the IDS paper
        print(len(cars))
        print("rule mining finished")

    
    #train IDS model
    ids = IDS()
    start = time.time()
    # all lambdas are set to the same value
    ids.fit(class_association_rules=cars, lambda_array=7*[1], quant_dataframe=quant_dataframe_train_disc, debug=False,random_seed=1)
    end = time.time()
    df_stat.loc["buildtime","ids"] = end-start
    #apply IDS model
    df_stat.loc["accuracy","ids"] =  ids.score(quant_dataframe_test_disc)    
    print("Acc IDS:",df_stat.loc["accuracy","ids"]  )
    df_stat.loc["rulecount","ids"] = len(ids.clf.rules)    
    antLengths=list(map(lambda r: len(r.car.antecedent.itemset.items()),ids.clf.rules ))
    df_stat.loc["rulelength","ids"]=sum(antLengths)/len(antLengths)
    
    avg_rule_legnth_ids = None
    print("Rule Count IDS:", df_stat.loc["rulecount","ids"])
    if (saveIDSRules):
        idsRulesPath = basepath+modelFolder+"/{}.csv".format(datasetname)
        file = open(idsRulesPath,"w")
        txtexport="rules,suppport,confidence,lift\n"
        #Before export, IDS sorts the rule by harmonic mean of support and confidence (st.hmean([self.car.support, self.car.confidence]))
        #In this order, rules are also applied for prediction
        for r in ids.clf.rules:
            args = [r.car.antecedent.string(), "{" + r.car.consequent.string() + "}", r.car.support, r.car.confidence,0]
            txtexport = txtexport+ "\"{} => {}\",{:.2f},{:.2f},{:.2f} \n".format(*args) 
        #add default rule
        classname = data_train_disc.columns.values[-1]
        txtexport = txtexport+ "\"{ } => " + "{"+classname+"="+ mode(data_train_disc[data_train_disc.columns[-1]])+ "}\", 0,0,0"

        print(txtexport)    
            
        file.write(txtexport)
        file.close()

    if (runQCBA):
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
        pred = qclf.predict(quant_dataframe_test_undisc) 
        
        #evaluate model - QCBA
        df_stat.loc["accuracy","idsqcba"] =  accuracy_score(actual, pred)    
        df_stat.loc["rulecount","idsqcba"] = len(rules)
        print("Acc IDS-QCBA:", df_stat.loc["accuracy","idsqcba"] )
        print("Rule Count IDS-QCBA:", df_stat.loc["rulecount","idsqcba"])
    return df_stat

def mean_allfolds(dataset_name, start=0, end=10, unique_transactions=True,runQCBA=False,useConfidenceForCandidateGeneration=True,saveIDSRules=True):
    files = [ dataset_name + repr(i) for i in range(start, end) ]
    df_agg = None
    emptyDF = True
    for file in files:        
        df_stats = run1fold(basepath,file, unique_transactions=unique_transactions,runQCBA=runQCBA,useConfidenceForCandidateGeneration=useConfidenceForCandidateGeneration,saveIDSRules=True)
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

datasets = ["iris","australian","anneal","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","ionosphere","labor","letter","lymph","segment","sonar","vehicle","vowel","hepatitis","spambase","hypothyroid"]    

runQCBA = False
saveIDSRules=True
useConfidenceForCandidateGeneration = True
separateFilePerDataset=False
modelFolder="IDS_Models"
if runQCBA:
    resultFolder="IDSQCBA_results"
else:
    resultFolder="IDS_results"

if not os.path.exists(modelFolder):
    os.makedirs(modelFolder)

if saveIDSRules and not os.path.exists(resultFolder):
    os.makedirs(resultFolder)

for dataset in datasets:
    print(dataset)
    
    # Check if already computed
    if separateFilePerDataset:
        resultFile=resultFolder+"/"+dataset+".csv"
        if os.path.exists(resultFile):
            print("skipping already computed")
            continue    
    else:
        resultFileIDS=resultFolder+"/IDS.csv"        
        # TODO: refactor this into functions
        if runQCBA:
            resultFileIDSQCBA=resultFolder+"/IDSQCBAPy.csv"
            if not os.path.exists(resultFileIDSQCBA):
                print("initializing result file")
                file = open(resultFileIDSQCBA,"w+")
                file.write("dataset,accuracy,rules,antlength,buildtime\n")
                file.close()   
            else:             
                file = open(resultFileIDSQCBA,"r+")
                strings = file.read()
                file.close()                
                if  "\n" + dataset +"," in strings:
                    print("skipping QCBA already computed")
                    continue
        if not os.path.exists(resultFileIDS):
            print("initializing result file")
            file = open(resultFileIDS,"w+")
            file.write("dataset,accuracy,rules,antlength,buildtime\n")
            file.close()
        else:
            file = open(resultFileIDS,"r+")   
            strings = file.read()
            file.close()
            if "\n" + dataset +"," in strings:
                print("skipping IDS already computed")
                continue        

    
    # Compute            
    df_stats_per_dataset = mean_allfolds(dataset, start=0,end=10, runQCBA=runQCBA,useConfidenceForCandidateGeneration=useConfidenceForCandidateGeneration,saveIDSRules=saveIDSRules)    
    print("*****")
    print(df_stats_per_dataset)
    print("******")

    # Save
    if separateFilePerDataset:
        df_stats_per_dataset.to_csv(resultFile)
    else:
        df_stats_per_dataset
        file = open(resultFileIDS,"a")
        file.write(dataset + "," + str(df_stats_per_dataset.loc["accuracy","ids"]) + "," + str(df_stats_per_dataset.loc["rulecount","ids"]) + "," + str(df_stats_per_dataset.loc["rulelength","ids"]) +"," + str(df_stats_per_dataset.loc["buildtime","ids"])  + "\n")
        file.close()
        if runQCBA:
            file = open(resultFileIDSQCBA,"a")
            file.write(dataset + "," + str(df_stats_per_dataset.loc["accuracy","idsqcba"]) + "," + str(df_stats_per_dataset.loc["rulecount","idsqcba"]) + "," + str(df_stats_per_dataset.loc["rulelength","idsqcba"])+"," + str(df_stats_per_dataset.loc["buildtime","idsqcba"])  + "\n")
            file.close()


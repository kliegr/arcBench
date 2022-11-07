# ARC bench
Benchmark suite for the QCBA framework. 

Prerequisites include Java 8, R and  qCBA package installed in R.

## Preparing data

Prerequisites include Python 2 with scikit-learn and pandas.

 ```
 generateFolds.sh
 ```
The benchmark uses standard open datasets from the UCI repository. To ensure that  all algorithm implementations operate on exactly the same folds, the folds are materialized.   Missing values are treated in both versions.

The output is saved into 
```
data/folds
```

The process may also create a temporary folder
```
data/output
```
This folder is removed on script completion. 

### Preparing data for IDS + CORELS
Create materialized discretized folds with `discretize_for_ids_corels.R`. This will create 

    data/folds_discr2/train/{dataset}{fold}.csv
    data/folds_discr2/train/{dataset}{fold}.cutpoints
    data/folds_discr2/test/{dataset}{fold}.csv
 
Note that the intervals are exactly the same as in `data/folds_discr/` only the interval format is different.

### Preparing data for scalability benchmarks

Scalability benchmarks are executed on subsets of the KDD'99 dataset, which are generated using:

    lib/materialize_folds_KDD.py

The results are saved to:

    data/datasets/kdd{subset}_.csv
    data/folds_nodiscr/train/kdd{subset}_{splitID}.csv
    data/folds_nodiscr/test/kdd{subset}_{splitID}.csv

## CBA + QCBA 
QCBA benchmark is run using functions defined in  `evalCBA_QCBA_all.R`.
If `evalCBA_QCBA_all.R` is run without parameters, it will list all predefined experiments. 

To run the main experiments, run `evalCBA_QCBA_all.sh`.

The experimentation framework is fail safe (`evalCBA_QCBA_all.R`) in that when interrupted and executed again, it will check the contents of `CBA_results` and skip reexecuting all dataset-experiment combinations for which the result are already logged. 


Discovered rules are stored into folders:

    CBA_models
    QCBA_models

For each configuration, the benchmark creates two files `-cba.csv` and `-qcba.csv` in 

    CBA_results

The first file contains evaluation results for CBA and the second  file for the postprocesing by QCBA. The difference between `117-noExtend-D-mci=-1-cba.csv` and all other `-cba.csv` runs  is that in run 117 the CBA execution includes default rule pruning (`-D-`). For all remaining runs, default rule pruning is not performed as part of CBA, but is performed as part of some QCBA runs, as indicated by `-Pcba-` in the filename. 


## SBRL + QCBA
QCBA benchmark is run using functions defined in  `evalSBRL_QCBA.R`.

Evaluation results on individual test datasets will be stored to:

    SBRL_results

The filename indicates one of {CBA,QCBA,SBRL,SBRLQCBA} and maxlength (maximum antecedent length) one of {1, Long} for maximum length of SBRL rules.
The Long option has different meaning for individual datasets. Refer to `evalSBRL_QCBA.R` for specific values.

Discovered rules are stored into folders:

    SBRL_Models/
    SBRL_QCBA_Models




 
## IDS + QCBA
This benchmark uses the IDS implementation at https://github.com/jirifilip/pyIDS.

As QCBA implementation, two options are supported:
* R implementation in https://github.com/kliegr/QCBA (default)
* Python implementation in https://github.com/jirifilip/pyARC  (experimental)

* Run `evalIDS.py`. Make sure that `runQCBA = False` is set inside the script. This will run pyIDS using the discretized folds created in the previous step. 
   
The model evaluation statistics against test data will be stored to:
 
    IDS_results/IDS.csv
 
 The IDS models will be stored to 
 
    IDS_Models/{dataset}{fold}.csv
    
* Run `evalIDS_QCBA.R`
This will run QCBA implementation in R, loading the IDS models from files in IDS_Models.
The IDS-QCBA models are stored to folder
 
      QCBA_IDS_Models
 
Evaluation results on individual test datasets are stored in folder:

    IDS_results

## PRM/FOIL2/CMAR/CPAR + QCBA
This benchmark uses the implemention of PRM, FOIL2, CMAR and CPAR from https://cran.r-project.org/web/packages/arulesCBA/index.html R package:

    evalCPAR_CMAR_PRM_FOIL2.R

Models are stored to:

    PRM_QCBA_Models
    FOIL2_QCBA_Models
    CPAR_QCBA_Models
    CMAR_QCBA_Models

Evaluation results are stored to

    PRM_results
    FOIL2_results
    CPAR_results
    CMAR_results

These scripts save intermediate evaluation results to temporary files (such as `temp_vowel_default_PRM_7_noPruning.csv`) in the project directory. These can be deleted.

## Benchmarks against PART, FURIA, J48
These benchmarks are implemented on top of the  WEKA framework in folder `WekaBench` (in Java) and are executed with:

    bash evalWEKA.sh 

Evaluation results are stored in folder

    WEKA_results

## Benchmark against CORELS
Requires https://github.com/corels/pycorels

    python evalCORELS.py
    
This scripts also adds one-hot-encoded data into 
```
data/folds/folds_discr_dumm
```

Evaluation results are saved to 

```
CORELS_results
```
Models are saved to 
```
CORELS_models
```

## Scalability benchmarks (CBA+QCBA) - KDD dataset


These are executed for two different thresholds of the minCI extension step:

    evalCBA_QCBA_KDDminc-1.sh
    evalCBA_QCBA_KDDminc0.sh

These scripts execute `evalCBA_QCBA_all.R`.

This experiment was run on a different more powerful machine than other experiments and the results have been placed into separate folders.

Discovered rules are stored in folders:

    CBA_models/server
    QCBA_models/server

For each configuration, the benchmark creates two files `-cba.csv` and `-qcba.csv` in 

    CBA_results/server

## Restricting experiments to one core.
To eliminate effect of multicore optimizations, computational resources can be restricted to using taskset as follows (Linux only):

    taskset -c 1 Rscript evalCPAR_CMAR_PRM_FOIL2.R --option
    taskset -c 0 ./evalCBA_QCBA_all.sh --option

The corresponding scripts are available as:

    single_core_evalCPAR_CMAR_PRM_FOIL2.sh
    single_core_evalQCBA_all.sh

Experiments with QCBA, CPAR, CMAR, PRM, FOIL2 were run using the single core version. 

## Analysis of results

To obtain aggregate results for all datasets:

    Rscript aggregateResults.R

This will create a `stats.csv` file in folders: 


    IDS_results
    CBA_results
    SBRL_results
    WEKA_results
    PRM_results
    FOIL2_results
    CPAR_results
    CMAR_results
    QCBA_eval_PRM_FOIL2_CMAR_CPAR #summary of effects of postprocessing by QCBA#5 on PRM,FOIL2,CMAR,CPAR 
    QCBA_eval_IDS_SBRL #summary of effects of postprocessing by QCBA#5 on IDS, SBRL and CBA

Additionally, this will create in project folder:

    accdifferencebydataset.csv
    
This file indicates on which datasets a model posprocessed by QCBA#5 won compared to a baseline method.
    

## Tables
    
The results from `stats.csv` files generated by `aggregateResults.R` are consolidated into tables 

    resultsTables.xlsx

## Figures


Figures (graphs, plots) are generated using

    plots.ipynb

### CBA+QCBA experiments - plots analysing results per dataset

The results of scalability experiments are read from `CBA_results`, `SBRL_results`,  `IDS_results`.

The resulting graphs are stored as pdf files to:

    SBRL_results/SBRLQCBA-noPruning-Long.pdf
    SBRL_results/SBRLQCBA-transactionBased-Long.pdf
    CBA_results/CBA_QCBA_198-numericOnly-T-Pcba-A-mci=-1.pdf
    CBA_results/CBA_QCBA_196-numericOnly-T-Pcba-A-transactionBased-mci=-1.pdf
    IDS_results/IDSQCBA_R_noPruning_ATTPRUNING_TRUE.pdf
    IDS_results/IDSQCBA_R_transactionBased_ATTPRUNING_TRUE.pdf
    

### Scalability experiments - plots analysing results

The results of scalability experiments are read from `CBA_results/server`, `SBRL_results/server`,  `IDS_results/server`.

The resulting graphs are stored as pdf files to:

    CBA_results/server/KDDablation.pdf
    CBA_results/server/KDD_CBA_QCBA_198-numericOnly-T-Pcba-A-mci=-1.pdf
    CBA_results/server/KDD_CBA_QCBA_198-numericOnly-T-Pcba-A-mci=0.pdf
    CBA_results/server/KDD_CBA_QCBA-196-numericOnly-T-Pcba-A-transactionBased-mci=0.pdf
    SBRL_results/SBRLQCBA-noPruning-Long.pdf
    IDS_results/server/KDD-IDSQCBA_R_noPruning_ATTPRUNING_TRUE.pdf

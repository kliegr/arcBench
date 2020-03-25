# ARC bench
Benchmark suite for the QCBA framework. 

Prerequisites include Java 8, R and  qCBA package installed in R.

## Preparing data

Prerequisites include Python 2 with sci-kit learn and pandas.

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

## CBA + QCBA 
QCBA benchmark is run using functions defined in  `evalCBA_QCBA_all.R`.
If evalCBA_QCBA_all.R is run without parameters, it will list all predefined experiments. 

To run only the main experiments, run `bash evalCBA_QCBA.sh`.

For each configuration, the benchmark creates two files .cba and .qcba in the `CBA_results` subfolder. The .cba file contains results for CBA output and QCBA for the QCBA model. The difference between .cba output of runs 117 and all other runs  is that in run 117 the CBA execution includes default rule pruning. For all remaining runs, default rule pruning is optionally performed within QCBA postpruning step.

Experiments can be run in parallel (recommended on server environment) by executing:
`bash runAll.sh`

The experimentation framework is fail safe in that when interrupted and executed again, it will pick up on the last dataset completely computed.


## SBRL + QCBA
* Run evalSBRL_QCBA.R

Results on individual test datasets will be stored to:

    SBRL_results/{algorithm}-{maxantlength}.csv

Where algorithm is one of {CBA,QCBA,SBRL,SBRLQCBA} and maxlength (maximum antecedent length) one of {1, Long}.
The settings of maxlength affects SBRL and consequently SBRLQCBA.
The Long option has different meaning for individual datasets. Refer to `evalSBRL_QCBA.R` for specific values.

## IDS + QCBA
This benchmark uses the IDS implementation at https://github.com/jirifilip/pyIDS.

As QCBA implementation, two options are supported:
* R implementation in https://github.com/kliegr/QCBA (default)
* Python implementation in https://github.com/jirifilip/pyARC  (experimental)

Executing experiments:
* Create materialized discretized folds with `discretize_for_ids`. This will create 

   data/folds_discr2/train/{dataset}{fold}.csv
   data/folds_discr2/train/{dataset}{fold}.cutpoints
   data/folds_discr2/test/{dataset}{fold}.csv
 
 Note that the intervals are exactly the same as in `data/folds_discr/` only the interval format is different.

 * Run `evalIDS_QCBA.py`. Make sure that `runQCBA = False` inside the script. This will run pyIDS using the discretized folds created in the previous step. 
 
 The IDS model will be stored to 
 
    IDS_Models/{dataset}{fold}.csv
   
 The model evaluation statistics against test data will be stored to:
 
    IDS_results/IDS.csv
    
 * Run `evalIDS_QCBA.R`
 This will run QCBA implementation in R, loading the IDS result from file.
 Results on individual test datasets will be stored to:

    IDS_results/IDSQCBA_R_{noPruning|transactionBased}.csv         



#### Alternative pure Python implementation 
* Run `evalIDS_QCBA.py`. Make sure that `runQCBA = True` inside the script.

Note that this feature is in development.


### Alternative benchmark of IDS - against reference implementation in https://github.com/lvhimabindu
The repository also contains scripts for benchmarking the IDS algorithm:

    IDS_deterministic_sensitivity_rule_count.py
    IDS_deterministic_sensitivity_data_size.py
    IDS_det_UCI.py

The first script bechmarks the time taken by IDS depending on the number of input rules. The second benchmark fixes the number of rules and varies the data size. The third script performs a proof of concept run on one fold from the UCI datasets. Due to the scalability limitations, this script times out on all datasets. 

The scripts expect that they are located in the same folder as checked out IDS repository (<a href="https://github.com/lvhimabindu/interpretable_decision_sets">official</a>, <a href="https://github.com/kliegr/interpretable_decision_sets">clone</a>).
The `IDS_deterministic_local.py` file needs to be "patched" by removing any code after the last function (`deterministic_local_search`) is defined. 


## Benchmarks against PART, FURIA, J48
These benchmarks are implemented on top of the  WEKA framework:

    bash evalWEKA.sh 

## Analysis of results

To obtain aggregate results for all datasets:

    Rscript aggregateResults.R

#### Alternative Python implementations

    python wontieloss_WEKA.py

    python summarize.py
   
Note that these scripts are obsololete / in development.


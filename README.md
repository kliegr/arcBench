# ARC bench
Benchmark suite for the QCBA framework. 

Prerequisites include Java 8, R and  qCBA package installed in R.

## Preparing data

Prerequisites include Python 2 with sci-kit learn and pandas.

 ```
 prepare_data.sh
 ```
The benchmark uses standard open datasets from the UCI repository. To ensure that  all algorithm implementations operate on exactly the same folds, the folds are materialized. Two versions of the folds are created, one without discretization of numerical attributes and one with it.  Missing values are treated in both versions.

The output is saved into 
```
data/folds
```

The process also creates a temporary folder
```
data/output
```
This folder is removed on script completion. 

## CBA and QCBA Benchmark - running 
QCBA benchmark is run using functions defined in  `evalQCBA.R`.
If evalQCBA.R is run without parameters, it will list all predefined experiments. 

To run experiments described in the thesis, use the following configuration numbers:

    Rscript evalQCBA.R 117 # refit (cba basline) 
    Rscript evalQCBA.R 120 # refit (#1) 
    Rscript evalQCBA.R 114 # refit + attribute pruning (#2)
    Rscript evalQCBA.R 42 # refit + attribute pruning  + trimming (#3)
    Rscript evalQCBA.R 186 # refit + attribute pruning  + trimming + extension (#4)
    Rscript evalQCBA.R 198 # refit + attribute pruning  + trimming + extension + postpruning  (#5)
    Rscript evalQCBA.R 196 # refit + attribute pruning  + trimming + extension + postpruning + transaction-based default overlap pruning  (#6)
    Rscript evalQCBA.R 197 #  refit + attribute pruning  + trimming + extension + postpruning + range-based default overlap pruning  (#7)

For each configuration, the benchmark creates two files .cba and .qcba in the `result` subfolder. The .cba file contains results for CBA output and QCBA for the QCBA model. The difference between .cba output of runs 117 and all other runs  is that in run 117 the CBA execution includes default rule pruning. For all remaining runs, default rule pruning is optionally performed within QCBA postpruning step.

All experiments can be run in parallel (recommended on server environment) by executing:
`bash runAll.sh`

The experimentation framework is fail safe in that when interrupted and executed again, it will pick up on the last dataset completely computed.

## CBA and QCBA Benchmark - analyzing
The results for individual configuration runs is saved into the `result` folder. A .csv file is created for each configuration and contains results for each dataset. 
To obtain aggregate results for all datasets, execute:

    python summarize.py
   
This will creat file `result/results.csv`, which contains one line for each configuration. 

## Benchmark of Interpretable Decision Sets
The repository also contains scripts for benchmarking the IDS algorithm:

    IDS_deterministic_sensitivity_rule_count.py
    IDS_deterministic_sensitivity_data_size.py
    IDS_det_UCI.py

The scripts do what the filename says: the first script bechmarks the time taken by IDS depending on the number of input rules. The second benchmark fixes the number of rules and varies the data size. The third script performs a proof of concept run on one fold from the UCI datasets. Due to the scalability limitations, this script times out on all datasets. 

### Setup. 
The scripts expect that they are located in the same folder as checked out IDS repository (<a href="https://github.com/lvhimabindu/interpretable_decision_sets">official</a>, <a href="https://github.com/kliegr/interpretable_decision_sets">clone</a>).
The `IDS_deterministic_local.py` file needs to be "patched" by removing any code after the last function (`deterministic_local_search`) is defined. 

The results are saved to the `results` folder as
    ids-rulecount.csv
    ids-datasize.csv
    ids-uci.csv

## Precomputed results
Note that the arcBench project is shipped with precomputed detailed and summary results in the `result` folder.  If you want to recompute the results, rename/remove these files.




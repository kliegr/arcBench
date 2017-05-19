# marcbench
Benchmark suite for the MARC framework

##Preparing data
Prerequisites include Python 2 with sci-kit learn and pandas.

 ```
 prepare_data.sh
 ```
The benchmark uses standard open datasets from the UCI repository. To ensure that  algorithm implementations in all platforms (Weka, R) operate on exactly the same folds, the folds are materialized. Two versions of the folds are created, one without discretization of numerical attributes and one with hit.  Missing values are treated in both versions.

The output is saved into 
```
data/folds
data/folds_nodiscr
```

The process also creates a temporary folder
```
data/output
```
## Benchmarks
There are two types of benchmarks: benchmark of accuracy and rule cound ("acc") and of algorithm runtime ("scaling").

Prerequisites include Java 8, R and  rMARC package installed in R.

### Running benchmarks - WEKA

Weka implementations of reference algorithms  of the two benchmark suites are executed using
```
run_WEKA_Bench_Acc.sh
run_WEKA_Bench_Scaling.sh
```
The output is stored into
```
/result
```
The won-tie-loss matrix and Wilcoxon signed rank test are executed using `wontieloss.py`

### Running benchmarks  - QCBA
QCBA benchmarks are run using functions defined in  `evalQCBA.R`
The scaling benchmark is run using `doEvalTime()`.
The default configuration for QCBA is run with `doEvalAccQCBA()` and for the experimental Multi Rule option with `doEvalAccQCBAMultiRule()`.
The output is stored into
```
/result
```

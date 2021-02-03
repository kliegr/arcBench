datasets="anneal;australian;autos;breast-w;colic;credit-a;credit-g;diabetes;glass;heart-statlog;hepatitis;hypothyroid;ionosphere;iris;labor;letter;lymph;segment;sonar;spambase;vehicle;vowel"

Rscript evalCBA_QCBA_all.R 117 -1 0 $datasets "" # baseline
Rscript evalCBA_QCBA_all.R 120 -1 0 $datasets "" # only refit
Rscript evalCBA_QCBA_all.R 114 -1 0 $datasets "" # + attribute removal
Rscript evalCBA_QCBA_all.R 42 -1 0 $datasets  "" # + trimming
Rscript evalCBA_QCBA_all.R 186 -1 0 $datasets "" # + extend
Rscript evalCBA_QCBA_all.R 198 -1 0 $datasets "" # + postpruning
Rscript evalCBA_QCBA_all.R 196 -1 0 $datasets "" # + default rule - transactio
Rscript evalCBA_QCBA_all.R 197 -1 0 $datasets "" # + default rule - range

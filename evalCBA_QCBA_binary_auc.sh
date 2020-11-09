Rscript evalCBA_QCBA_all.R 120 -1 0 "hepatitis;ionosphere;sonar;spambase;australian;breast-w;colic;credit-a;diabetes;heart-statlog;credit-g" "_auc_ordered" # only refit
Rscript evalCBA_QCBA_all.R 114 -1 0 "hepatitis;ionosphere;sonar;spambase;australian;breast-w;colic;credit-a;diabetes;heart-statlog;credit-g" "_auc_ordered"  # + attribute removal
Rscript evalCBA_QCBA_all.R 42 -1 0 "hepatitis;ionosphere;sonar;spambase;australian;breast-w;colic;credit-a;diabetes;heart-statlog;credit-g" "_auc_ordered"  # + trimming
Rscript evalCBA_QCBA_all.R 186 -1 0 "hepatitis;ionosphere;sonar;spambase;australian;breast-w;colic;credit-a;diabetes;heart-statlog;credit-g" "_auc_ordered" # + extend
Rscript evalCBA_QCBA_all.R 196 -1 0 "hepatitis;ionosphere;sonar;spambase;australian;breast-w;colic;credit-a;diabetes;heart-statlog;credit-g" "_auc_ordered" # + default rule - transactio
Rscript evalCBA_QCBA_all.R 197 -1 0 "hepatitis;ionosphere;sonar;spambase;australian;breast-w;colic;credit-a;diabetes;heart-statlog;credit-g" "_auc_ordered" # + default rule - range
Rscript evalCBA_QCBA_all.R 117 -1 0 "hepatitis;ionosphere;sonar;spambase;australian;breast-w;colic;credit-a;diabetes;heart-statlog;credit-g" "_auc_ordered"  # baseline
Rscript evalCBA_QCBA_all.R 198 -1 0 "hepatitis;ionosphere;sonar;spambase;australian;breast-w;colic;credit-a;diabetes;heart-statlog;credit-g" "_auc_ordered"  # + postpruning

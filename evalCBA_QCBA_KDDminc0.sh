datasets="kdd1000_;kdd10000_;kdd20000_;kdd30000_;kdd40000_;kdd50000_"

Rscript evalCBA_QCBA_all.R 117 0 0 $datasets "" # baseline
Rscript evalCBA_QCBA_all.R 120 0 0 $datasets "" # only refit
Rscript evalCBA_QCBA_all.R 114 0 0 $datasets "" # + attribute removal
Rscript evalCBA_QCBA_all.R 42 0 0 $datasets  "" # + trimming
Rscript evalCBA_QCBA_all.R 186 0 0 $datasets "" # + extend
Rscript evalCBA_QCBA_all.R 198 0 0 $datasets "" # + postpruning
Rscript evalCBA_QCBA_all.R 196 0 0 $datasets "" # + default rule - transactio

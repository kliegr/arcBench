datasets="kdd1000_;kdd10000_;kdd20000_;kdd30000_;kdd40000_;kdd50000_"

Rscript evalCBA_QCBA_all.R 186 -1 0 $datasets "" # + extend
Rscript evalCBA_QCBA_all.R 198 -1 0 $datasets "" # + postpruning
Rscript evalCBA_QCBA_all.R 196 -1 0 $datasets "" # + default rule - transactio

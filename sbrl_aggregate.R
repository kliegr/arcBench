library(tidyverse)
library(data.table)

aggregate <- function(basepath) {
  
  tbl <-
    list.files(path = paste("./",basepath,sep=""),pattern = "*.csv", 
               full.names = T) %>% 
    map_df(~read_csv(.))
  tbl <- select(tbl,-c("CBA","QCBA"))
  aggr<- tbl  %>% group_by(X1) %>% summarize_all(mean)
  sbrlqcbawon <-sum(tbl[tbl$X1=="accuracy","SBRLQCBA"] >  tbl[tbl$X1=="accuracy","SBRL"])
  sbrlqcbatie <-sum(tbl[tbl$X1=="accuracy","SBRLQCBA"] ==  tbl[tbl$X1=="accuracy","SBRL"])
  sbrlqcbaloss <-sum(tbl[tbl$X1=="accuracy","SBRLQCBA"] <  tbl[tbl$X1=="accuracy","SBRL"])
  aggr[5,] <- c("modelsize",aggr[4,2:3] * aggr[3,2:3])  
  aggr[6,] <- c("won/tie/loss", "", paste(sbrlqcbawon,"/",sbrlqcbatie,"/",sbrlqcbaloss))  
  write_csv(aggr,paste(basepath,".csv",sep=""))

}
aggregate("transactionBased-1")
aggregate("noPruning-1")
aggregate("noPruning-Long")
aggregate("transactionBased-Long")


 

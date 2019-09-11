library(tidyverse)
library(data.table)

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = gsub("(.*/)([^//]+?)(.csv)", "\\2", flnm))
}

aggregate <- function(basefolder,experiment) {
  
  tbl <-
    list.files(path = paste("./",basefolder, "/", experiment,sep=""),pattern = "*.csv", 
               full.names = T) %>% 
    map_df(~read_plus(.))
  acc <- tbl %>% group_by(filename) %>% do(.[1,"SBRL"]) %>% rename(dataset=filename,accuracy=SBRL)
  write_csv(acc,paste(basefolder,"/", "SBRL","-",experiment, "-byDataset.csv",sep=""))
  acc <- tbl %>% group_by(filename) %>% do(.[1,"SBRLQCBA"]) %>% rename(dataset=filename,accuracy=SBRLQCBA)
  write_csv(acc,paste(basefolder,"/", "SBRLQCBA","-", experiment,  "-byDataset.csv",sep=""))
  
  tbl <- select(tbl,-c("CBA","QCBA","filename"))
  aggr<- tbl  %>% group_by(X1) %>% summarize_all(mean)
  sbrlqcbawon <-sum(tbl[tbl$X1=="accuracy","SBRLQCBA"] >  tbl[tbl$X1=="accuracy","SBRL"])
  sbrlqcbatie <-sum(tbl[tbl$X1=="accuracy","SBRLQCBA"] ==  tbl[tbl$X1=="accuracy","SBRL"])
  sbrlqcbaloss <-sum(tbl[tbl$X1=="accuracy","SBRLQCBA"] <  tbl[tbl$X1=="accuracy","SBRL"])
  aggr[5,] <- c("modelsize",aggr[4,2:3] * aggr[3,2:3])  
  aggr[6,] <- c("won/tie/loss", "", paste(sbrlqcbawon,"/",sbrlqcbatie,"/",sbrlqcbaloss))  
  write_csv(aggr,paste(basefolder,"/", experiment, ".csv",sep=""))
  
}
aggregate("SBRL_results","transactionBased-1")
aggregate("SBRL_results","noPruning-1")
aggregate("SBRL_results","noPruning-Long")
aggregate("SBRL_results","transactionBased-Long")




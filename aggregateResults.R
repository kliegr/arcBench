library(tidyverse)
wontieloss <- function (baseforWontieloss,basefolder,filenames,col_names,extrastats=TRUE, decreaseInModelSize=FALSE)
{
  statistics<-c("accuracy (macro average)","won/tie/loss", "p-value (accuracy difference)")
  if (extrastats)
  {
    statistics <-  c(statistics, "avg number of rules","avg conditions / rule","avg conditions / model","median build time [s]", "avg build time norm")  
  }
  if (decreaseInModelSize)
  {
    statistics <-  c(statistics,"decrease in model size (avg)", "decrease in model size (max)","decrease in model size (min)")
  }
  result <- data.frame(matrix(rep(0,length(col_names)*length(statistics)), ncol = length(col_names), nrow = length(statistics)), row.names = statistics)
  df_base <- NULL
  colnames(result) <- col_names
  col=0
  for (filename in filenames)
  {
    message(filename)
    col=col+1
    df<-read_csv(paste(basefolder,"/",filename,sep=""))
    result[1,col]<-round(mean(df$accuracy),2)
    if (baseforWontieloss[col])
    {
      result[2,col] <- ""
      df_base <- df
    }
    else{
      colnames(df)
      colnames(df_base)
      merged <- merge(df,df_base,by="dataset",suffixes=c("_QCBA","_base"))
      merged$accuracy_QCBA<-round(merged$accuracy_QCBA,2)
      merged$accuracy_base<-round(merged$accuracy_base,2)
      pValue <- wilcox.test(merged$accuracy_QCBA, merged$accuracy_base, paired=TRUE)$p.value
      qcbawon <-sum(merged$accuracy_QCBA>merged$accuracy_base)
      qcbatie <-sum(merged$accuracy_QCBA==merged$accuracy_base)
      qcbaloss <-sum(merged$accuracy_QCBA<merged$accuracy_base)
      result[2,col] <- c(paste0(qcbawon,"/",qcbatie,"/",qcbaloss))  
      result[3,col] <- round(pValue,5)
      if(decreaseInModelSize)
      {
        result[4,col]<-paste(round(1-mean(merged$rules_QCBA * merged$antlength_QCBA)/(mean(merged$rules_base  * merged$antlength_base)),2)*100, "%")
        result[5,col]<-paste(round(1-min((merged$rules_QCBA * merged$antlength_QCBA)/(merged$rules_base  * merged$antlength_base)),2)*100, "%")
        result[6,col]<-paste(round(1-max((merged$rules_QCBA * merged$antlength_QCBA)/(merged$rules_base  * merged$antlength_base)),2)*100, "%")
      }
    }
    if (extrastats)
    {
      result[4,col]<-round(mean(df$rules),1)
      result[5,col]<-round(mean(df$antlength),1)
      result[6,col]<-round(mean(df$rules)*mean(df$antlength),1)
      buildtime<-round(median(df$buildtime),1)
      result[7,col]<-buildtime
      if (baseforWontieloss[col])
      {
        result[8,col]<-1.0
        buildtimeRef <-mean(df$buildtime)
      }
      else 
      {
        result[8,col]<-round(mean(df$buildtime)/buildtimeRef,2)
      }
    }
  }  
  write.csv(result,paste(basefolder,"/stats.csv",sep=""))
  return(result)
}


# CBA 
baseforWontieloss<-c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
basefolder<-"CBA_results"
filenames<-c("117-noExtend-D-mci=0-cba.csv","120-noExtend-mci=0-qcba.csv", "114-noExtend-A-mci=0-qcba.csv", "42-noExtend-T-A-mci=0-qcba.csv", "186-numericOnly-T-A-mci=0-qcba.csv",  "198-numericOnly-T-Pcba-A-mci=0-qcba.csv", "196-numericOnly-T-Pcba-A-transactionBased-mci=0-qcba.csv", "197-numericOnly-T-Pcba-A-rangeBased-mci=0-qcba.csv")
col_names<-c("only CBA","CBA+QCBA #1 (+ refit)","CBA+QCBA #2 (+att pruning)", "CBA+QCBA #3 (+trimming)","CBA+QCBA #4 (+extension)","CBA+QCBA #5 (+postpruning)","CBA+QCBA #6 (+tran. based pruning)","CBA+QCBA #7 (+rangeBased pruning)")
result<-wontieloss(baseforWontieloss,basefolder,filenames,col_names)
result


# SBRL 
#SBRL+QCBA is compared against SBRL only,
#i.e. TRUE means that SBRL-1.csv will be used as a base accuracy to compare against for both SBRLQCBA-noPruning-1.csv and SBRLQCBA-transactionBased-1.csv
baseforWontieloss<-c(TRUE,FALSE,FALSE,TRUE,FALSE,FALSE)
basefolder<-"SBRL_results"
filenames<-c("SBRL-1.csv","SBRLQCBA-noPruning-1.csv", "SBRLQCBA-transactionBased-1.csv","SBRL-Long.csv", "SBRLQCBA-noPruning-Long.csv","SBRLQCBA-transactionBased-Long.csv")
col_names<-c("only SBRL (Short)","SBRL+QCBA (Short) #5","SBRL+QCBA (Short) #6","only SBRL (long)","SBRL+QCBA (long) #5","SBRL+QCBA (long) #6")
result<-wontieloss(baseforWontieloss,basefolder,filenames,col_names)
result
# IDS
baseforWontieloss<-c(TRUE,FALSE,FALSE)
basefolder<-"IDS_results"
filenames<-c("IDS.csv","IDSQCBA_R_noPruning.csv", "IDSQCBA_R_transactionBased.csv")
col_names<-c("only IDS","IDS+QCBA #5","IDS+QCBA #6")
result<-wontieloss(baseforWontieloss,basefolder,filenames,col_names)
result



# summary of effects of postprocessing by QCBA#5 
baseforWontieloss<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE)
basefolder<-"."
filenames<-c( "CBA_results/117-noExtend-D-mci=0-cba.csv","CBA_results/198-numericOnly-T-Pcba-A-mci=0-qcba.csv", "SBRL_results/SBRL-Long.csv", "SBRL_results/SBRLQCBA-noPruning-Long.csv", "IDS_results/IDS.csv", "IDS_results/IDSQCBA_R_noPruning.csv"  )
col_names<-c("CBA","CBA+QCBA#5","SBRL","SBRL+QCBA#5" ,"IDS","IDS+QCBA#5")
result<-wontieloss(baseforWontieloss,basefolder,filenames,col_names,extrastats=FALSE,decreaseInModelSize=TRUE)
result


# QCBA#5 against other symbolic learners
baseforWontieloss<-c(TRUE,FALSE,FALSE,FALSE,FALSE)
basefolder<-"WEKA_results"
filenames<-c("../CBA_results/198-numericOnly-T-Pcba-A-mci=0-qcba.csv",  "J48-accuracy.csv","PART-accuracy.csv", "RIPPER-accuracy.csv", "FURIA-accuracy_missing_omitted.csv")
col_names<-c("CBA+QCBA #5","J48","PART","RIPPER", "FURIA")
result<-wontieloss(baseforWontieloss,basefolder,filenames,col_names,extrastats=FALSE)
result



basefolder<-"SBRL_results"
result <- data.frame(matrix(rep(0,20), ncol = 4, nrow = 5), row.names = c("accuracy (macro average)","won/tie/loss","avg number of rules","avg conditions / rule","avg conditions / model"))
df_base <- NULL
colnames(result)<-c("only SBRL (1)","SBRL+QCBA (1)","only SBRL (long)","SBRL+QCBA (long)")
filenames<-c("SBRL-1.csv","SBRLQCBA-1.csv","SBRL-Long.csv","SBRLQCBA-Long.csv")
baseforWontieloss<-c(TRUE,FALSE,TRUE,FALSE,FALSE)
col=0
for (filename in filenames)
{
  col=col+1
  df<-read_csv(paste(basefolder,"/",filename,sep=""))
  result[1,col]<-round(mean(df$accuracy),2)
  if (baseforWontieloss[col])
  {
    result[2,col] <- ""
    df_base <- df
  }
  else{
    merged <- merge(df,df_base,by="dataset",suffixes=c("_QCBA","_base"))
    merged$accuracy_QCBA<-round(merged$accuracy_QCBA,2)
    merged$accuracy_base<-round(merged$accuracy_base,2)
    sbrlqcbawon <-sum(merged$accuracy_QCBA>merged$accuracy_base)
    sbrlqcbatie <-sum(merged$accuracy_QCBA==merged$accuracy_base)
    sbrlqcbaloss <-sum(merged$accuracy_QCBA<merged$accuracy_base)
    result[2,col] <- c(paste(sbrlqcbawon,"/",sbrlqcbatie,"/",sbrlqcbaloss))  
    df_base <- NULL
  }
  result[3,col]<-round(mean(df$rules),1)
  result[4,col]<-round(mean(df$antlength),1)
  result[5,col]<-round(mean(df$rules)*mean(df$antlength),1)
}
result



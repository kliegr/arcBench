library(arc)
dataset_path <- "data/folds/train/lymph0.csv"
#data with discretization applied by external preprocessing
trainFold <- utils::read.csv(dataset_path, header = TRUE, check.names = FALSE)
trainFold[,9] <- discretize(trainFold[,9], "frequency", categories=3)
trainFold[,10] <- discretize(trainFold[,10], "frequency", categories=3)
trainFold[,18] <- discretize(trainFold[,18], "frequency", categories=3)
classAtt <- "class"
outputFileName <- "result/arc-data-size.csv"
appearance <- arc::getAppearance(trainFold, classAtt)
write(paste("dataset,input rules,output rules,input rows,time"), file = outputFileName,
      ncolumns = 1,
      append = FALSE, sep = ",")

rule_count=100
#we gradually increase number of input rules and observe how run time changes
trainFold_oversampled <- trainFold
for (i in seq(1,1000))
{
  # double the dataset on each iteration
  trainFold_oversampled <- rbind(trainFold_oversampled,trainFold_oversampled)
  

  
  txns_discr <- as(trainFold_oversampled, "transactions")
  #this returns a lot of rules (4187880) to choose from
  #rule learning is performed on discretized data
  rules <- apriori(trainFold_oversampled, parameter =
                     list(confidence = 0, support= 0.01, minlen=1, maxlen=4), appearance=appearance)
  subs_rules<-rules[0:rule_count]
    
    message(paste("iteration",i))
    # we do ten iterations to have a more robust estimate
    ptm <- proc.time()
    for (j in 1:10)
    {
        rmCBA <- cba_manual(trainFold_oversampled,subs_rules, txns_discr, appearance$rhs,
                    classAtt, cutp= list(), pruning_options=NULL)
    }
    proctime<- proc.time() - ptm
    dur<-proctime[3]/10
    message (paste("CBA building (arc) took:", dur, " seconds"))
    write(paste(dataset_path,  length(subs_rules), length(rmCBA@rules), nrow(trainFold_oversampled), dur, sep = ","), file = outputFileName,
          ncolumns = 1,
          append = TRUE, sep = ",")
}

library(arc)
library(rCBA)
dataset_path <- "data/folds/train/lymph0.csv"
#data with discretization applied by external preprocessing
trainFold <- utils::read.csv(dataset_path, header = TRUE, check.names = FALSE)
trainFold[,9] <- discretize(trainFold[,9], "frequency", categories=3)
trainFold[,10] <- discretize(trainFold[,10], "frequency", categories=3)
trainFold[,18] <- discretize(trainFold[,18], "frequency", categories=3)
classAtt <- "class"
outputFileName <- "result/arc-data-size.csv"
appearance <- arc::getAppearance(trainFold, classAtt)
write(paste("dataset,input rows,input rules,output_rules_arc,output_rules_rcba,time_arc,time_rcba"), file = outputFileName,
      ncolumns = 1,
      append = FALSE, sep = ",")

rule_count=100
#we gradually increase the number of input rows and observe how run time changes
trainFold_oversampled <- trainFold
for (i in seq(1,1000))
{
  # double the dataset on each iteration
  trainFold_oversampled <- rbind(trainFold_oversampled,trainFold_oversampled)
  txns_discr <- as(trainFold_oversampled, "transactions")

    rules <- apriori(trainFold_oversampled, parameter =
                     list(confidence = 0, support= 0.01, minlen=1, maxlen=4), appearance=appearance)
  subs_rules<-rules[0:rule_count]
    
#arc start
    # we do ten iterations to have a more robust estimate
    ptm <- proc.time()
    for (j in 1:10)
    {
        rmCBA <- cba_manual(trainFold_oversampled,subs_rules, txns_discr, appearance$rhs,
                    classAtt, cutp= list(), pruning_options=NULL)
    }
    proctime<- proc.time() - ptm
    dur_arc<-proctime[3]/10
#arc end
#rCBA start
    ptm <- proc.time()
    for (j in 1:10)
    {
      rmRCBA <- rCBA::pruning(trainFold_oversampled, subs_rules, method="m2cba")
    }
    proctime<- proc.time() - ptm
    dur_rcba<-proctime[3]/10

#rCBA end
    write(paste(dataset_path, nrow(trainFold_oversampled), length(subs_rules), length(rmCBA@rules), nrow(rmRCBA), dur_arc, dur_rcba, sep = ","), file = outputFileName,
          ncolumns = 1,
          append = TRUE, sep = ",")
}

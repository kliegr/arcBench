library(arc)
dataset_path="data/folds/train/lymph0.csv"
#data with discretization applied by external preprocessing
trainFold <- utils::read.csv(dataset_path, header = TRUE, check.names = FALSE)
trainFold[,9] <- discretize(trainFold[,9], "frequency", categories=3)
trainFold[,10] <- discretize(trainFold[,10], "frequency", categories=3)
trainFold[,18] <- discretize(trainFold[,18], "frequency", categories=3)
rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE)
pruning_options=NULL
classAtt = "class"

appearance <- arc::getAppearance(trainFold, classAtt)
txns_discr <- as(trainFold, "transactions")
#this returns a lot of rules (4187880) to choose from
#rule learning is performed on discretized data
rules <- apriori(txns_discr, parameter =
                   list(confidence = 0, support= 0.01, minlen=1, maxlen=20), appearance=appearance)
write(paste("dataset,input rules,output rules,time"), file = "cba.csv",
      ncolumns = 1,
      append = FALSE, sep = ",")

#we gradually increase number of input rules and observe how run time changes
for (i in c(10:19,seq(20,100,by=10),seq(200,1000,by=100),seq(2000,10000,by=1000),seq(20000,100000,by=10000)))
{
    message(i)
    start.time <- Sys.time()
    # we do ten iterations to have a more robust estimate
    for (j in 1:10)
    {
        rmCBA <- cba_manual(trainFold,rules[0:i], txns_discr, appearance$rhs,
                    classAtt, cutp= list(), pruning_options=NULL)
    }
    end.time <- Sys.time()
    dur <-round((end.time - start.time)/10,3)
    message (paste("CBA building (arc) took:", dur, " seconds"))
    write(paste(dataset_path, i, length(rmCBA@rules), dur, sep = ","), file = "result/arc-rule-sensitivity.csv",
          ncolumns = 1,
          append = TRUE, sep = ",")
}

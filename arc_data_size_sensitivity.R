#library(devtools)
#install_github('ianjjohnson/arulesCBA', ref="time-trials", force=TRUE)
#install_github('jaroslav-kuchar/rCBA', ref="develop")
#install_github('kliegr/arc', force=TRUE)

library(rJava)
.jinit(force.init = TRUE, parameters="-Xmx16g")

library(arc)
library(rCBA)
library(arulesCBA)
dataset_path <- "data/folds/train/lymph0.csv"
#data with discretization applied by external preprocessing
trainFold <- utils::read.csv(dataset_path, header = TRUE, check.names = FALSE)
trainFold[,9] <- as.factor(trainFold[,9])
trainFold[,10] <- as.factor(trainFold[,10])
trainFold[,18] <- as.factor(trainFold[,18])
disc.data <- lapply(trainFold, levels)
classAtt <- "class"
outputFileName <- "result/arc-data-size.csv"
appearance <- arc::getAppearance(trainFold, classAtt)
write(paste("dataset,input rows,input rules,output_rules_arc,output_rules_acba,output_rules_rcba,time_arc,time_acba,time_rcba"), file = outputFileName,
      ncolumns = 1,
      append = FALSE, sep = ",")

rule_count=100
#we gradually increase the number of input rows and observe how run time changes
trainFold_oversampled <- trainFold
number_of_iterations <- 1
for (i in seq(1,14))
{
  # double the dataset on each iteration
  trainFold_oversampled <- rbind(trainFold_oversampled,trainFold_oversampled)
  message(nrow(trainFold_oversampled))
#  if (nrow(trainFold_oversampled)<500000)
#  {
#    next
#  }
  txns_discr <- as(trainFold_oversampled, "transactions")

    rules <- apriori(trainFold_oversampled, parameter =
                     list(confidence = 0, support= 0.01, minlen=1, maxlen=4), appearance=appearance)
  subs_rules<-rules[0:rule_count]
    
#arc start
    # we do ten iterations to have a more robust estimate
    ptm <- proc.time()
    for (j in 1:number_of_iterations)
    {
        rmCBA <- cba_manual(trainFold_oversampled,subs_rules, txns_discr, appearance$rhs,
                    classAtt, cutp= list(), pruning_options=NULL)
    }
    proctime<- proc.time() - ptm
    dur_arc<-proctime[3]/number_of_iterations
#arc end
 
#rCBA start
    ptm <- proc.time()
    for (j in 1:number_of_iterations)
    {
      rmRCBA <- rCBA::pruning(trainFold_oversampled, subs_rules, method="m2cba")
      
      J("java.lang.System")$gc()
      gc()
    }
    proctime<- proc.time() - ptm
    dur_rcba<-proctime[3]/number_of_iterations
#rCBA end
#arulesCBA start      
    ptm <- proc.time()
    for (j in 1:number_of_iterations)
    {
      message("starting arulesCBA")
      rmArulesCBA <- arulesCBA::CBA.internal(subs_rules, txns_discr, classAtt, disc.data, method="CBA",sort.rules = TRUE)
      message(paste("number of input rules:",rule_count,", pruned rules in arulesCBA model:", length(rmArulesCBA$rules)+1))
      message("arulesCBA finished")
    }
    proctime<- proc.time() - ptm
    dur_acba<-proctime[3]/number_of_iterations
    message(paste("acba finished"))
#arulesCBA end              
        write(paste(dataset_path, nrow(trainFold_oversampled), length(subs_rules), length(rmCBA@rules),  length(rmArulesCBA$rules)+1,nrow(rmRCBA), dur_arc, dur_acba, dur_rcba , sep = ","), file = outputFileName,
          ncolumns = 1,
          append = TRUE, sep = ",")
}

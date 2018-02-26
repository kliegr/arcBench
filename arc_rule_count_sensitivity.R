#library(devtools)
#install_github('ianjjohnson/arulesCBA', ref="time-trials", force=TRUE)
#install_github('jaroslav-kuchar/rCBA', ref="develop")

library(arc)
library(rCBA)
library(arulesCBA)

dataset_path="data/folds/train/lymph0.csv"
#data with discretization applied by external preprocessing
trainFold <- utils::read.csv(dataset_path, header = TRUE, check.names = FALSE)
trainFold[,9] <- discretize(trainFold[,9], "frequency", categories=3)
trainFold[,10] <- discretize(trainFold[,10], "frequency", categories=3)
trainFold[,18] <- discretize(trainFold[,18], "frequency", categories=3)

disc.data <- lapply(trainFold, levels)
classAtt <- "class"
outputFileName<-"result/arc-rule-sensitivity.csv"
appearance <- arc::getAppearance(trainFold, classAtt)
txns_discr <- as(trainFold, "transactions")
#this returns a lot of rules (4187880) to choose from
#rule learning is performed on discretized datlibrary(microbenchmark::microbenchmark)a
rules <- apriori(txns_discr, parameter =
                   list(confidence = 0, support= 0.01, minlen=1, maxlen=20), appearance=appearance)
message(paste("number of rules to choose from", length(rules)))
write(paste("dataset,input rules,output_rules_arc,output_rules_rcba,output_rules_acba,time_arc,time_rcba,time_acba"), file = outputFileName,
      ncolumns = 1,
      append = FALSE, sep = ",")

number_of_iterations=1
#we gradually increase number of input rules and observe how run time changes

for (i in c(10:19,seq(20,100,by=10),seq(200,1000,by=100),seq(2000,10000,by=1000),seq(20000,100000,by=10000)))
{
    message(paste("iteration",i))
    # we do ten iterations to have a more robust estimate
  
# start of arc
    ptm <- proc.time()
    for (j in 1:number_of_iterations)
    {
      message("arc starting")
        rmCBA <- cba_manual(trainFold,rules[0:i], txns_discr, appearance$rhs,
                    classAtt, cutp= list(), pruning_options=NULL)
        message("arc finished")
    }
    proctime<- proc.time() - ptm
    dur_arc<-proctime[3]/number_of_iterations # proctime[3] returns cumulative sum of user times (https://stat.ethz.ch/R-manual/R-devel/library/base/html/proc.time.html)
# end of arc
# start of rCBA
    ptm <- proc.time()
    for (j in 1:number_of_iterations)
    {
      message("starting rCBA")
      rmRCBA <- rCBA::pruning(trainFold, rules[0:i], method="m2cba")
      message("finished rCBA")
    }
    proctime<- proc.time() - ptm
    dur_rcba<-proctime[3]/number_of_iterations
# end of rCBA
# start of arulesCBA
    ptm <- proc.time()
    for (j in 1:number_of_iterations)
    {
      message("starting arulesCBA")
      rmArulesCBA <- arulesCBA::CBA.internal(rules[0:i], txns_discr, classAtt, disc.data, method="CBA",sort.rules = TRUE)
      message(paste("number of input rules:",i,", pruned rules in arulesCBA model:", length(rmArulesCBA$rules)))
      message("arulesCBA finished")
    }
    proctime<- proc.time() - ptm
    dur_acba<-proctime[3]/number_of_iterations
    message(paste("acba finished"))
# end of arulesCBA           
    write(paste(dataset_path, i, length(rmCBA@rules),  nrow(rmRCBA), length(rmArulesCBA$rules), dur_arc, dur_rcba, dur_acba, sep = ","), file = outputFileName,
          ncolumns = 1,
          append = TRUE, sep = ",")
}

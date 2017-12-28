library(qCBA)
library(stringr)
args <- commandArgs(trailingOnly = TRUE)
onlyList <-FALSE

# will store CBA result file and a QCBA configuration file ot the debug foler
# QCBA can then be debugged by running the java project in an IDE using
# java -jar AC1.jar config.xml
debug <- FALSE

minCondImprovement=-1
minImprovement = 0

if (is.null(args))
{
  # default to nonexistent configuration  if no arguments are passed
  args = c(-1)
} else if (length(args)==3) {
  minCondImprovement = args[2]
  minImprovement = args[3]
}
message(paste("Using minCondImprovement", minCondImprovement))
message(paste("Using minImprovement", minImprovement))

experimentToRun=args[1]
experimentToRun = -2 #198

foldsToProcess <- 10

logger<-"WARNING"
global_createHistorySlot<-FALSE
datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
#common settings
global_maxtime <- 1000
global_target_rule_count=50000
global_fuzzification <- FALSE
global_annotate <- FALSE
global_testingType <- "oneRule"
global_minConf = 0.5
global_extensionStrategy="ConfImprovementAgainstLastConfirmedExtension"
global_continuousPruning	<- FALSE


evalAutoFitQCBA <- function(rmCBA,trainFold,classAtt){

  bestModel <-NULL
  bestModelAcc <- -1

  NextendType = c("noExtend","numericOnly")
  NattributePruning	<-	c(TRUE,FALSE)
  NcontinuousPruning	<-	c(TRUE,FALSE)
  Npostpruning	<-	c("none","cba")
  Ndefault_rule_pruning	<-	c(TRUE,FALSE)
  Ntrim_literal_boundaries		<-	c(TRUE,FALSE)
  NdefaultRuleOverlapPruning  <- c("transactionBased","noPruning")
  fuzzification <-FALSE
  annotate <-FALSE
  testingType <- "oneRule"
  combination_no <- 1 #max 196
  for (extendType in NextendType){
    for (trim_literal_boundaries in Ntrim_literal_boundaries) {
      for (continuousPruning in NcontinuousPruning ) {
        for (postpruning in Npostpruning ) {
          for (attributePruning in  NattributePruning) {
            for (default_rule_pruning in Ndefault_rule_pruning) {
              for (defaultRuleOverlapPruning in NdefaultRuleOverlapPruning) {
                experiment_name<-experimentName(combination_no,extendType,default_rule_pruning,attributePruning,trim_literal_boundaries,continuousPruning,postpruning,defaultRuleOverlapPruning,minCondImprovement,minImprovement)
                message(experiment_name)
                  rmQCBA <- qcba(cbaRuleModel=rmCBA,datadf=trainFold,extend=extendType,defaultRuleOverlapPruning=defaultRuleOverlapPruning,attributePruning=attributePruning,trim_literal_boundaries=trim_literal_boundaries,
                                 continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=fuzzification, annotate=annotate,minImprovement=minImprovement,
                                 minCondImprovement=minCondImprovement,  createHistorySlot=global_createHistorySlot,
                                 loglevel = logger)
                  prediction <- predict(rmQCBA,trainFold,testingType=testingType)
                  accuracy <- CBARuleModelAccuracy(prediction, trainFold[[classAtt]])
                  if (accuracy>bestModelAcc)
                  {
                    message(paste("Current best with acc",accuracy))
                    bestModelAcc<-accuracy
                    bestModel <- rmQCBA
                  }
                }
                combination_no<-combination_no+1
            }
          }
        }
      }
    }
  }
  return(bestModel)
}


evalTimeQCBA <- function(trainFold,testFold,foldsize,logpath,iterations,includeQCBA)
{
  classAtt <- colnames(trainFold)[length(trainFold)]
  start.time <- Sys.time()
  for (i in 1:iterations)  rmCBA <- cba(trainFold, classAtt=classAtt,rulelearning_options=list(minsupp=0.001, minconf=0.1, minlen=1, maxlen=30, maxtime=1, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE),
                                        pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE))
  end.time <- Sys.time()
  buildTime <- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
  print(paste("CBA build time",buildTime))
  start.time <- Sys.time()
  for (i in 1:iterations)  prediction <- predict(rmCBA,testFold)
  accuracy <- CBARuleModelAccuracy(prediction, testFold[[classAtt]])
  end.time <- Sys.time()
  predictTime <- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)

  write(c(foldsize,buildTime,predictTime,accuracy, "CBA"), file = logpath,
        ncolumns = 5,
        append = TRUE, sep = ",")

  if (!includeQCBA) return()
  .jinit(force.init = TRUE,parameters="-Xmx4g")
  start.time <- Sys.time()

  for (qcbaConfID in 1:7)
  {
    if (qcbaConfID==1)
    {
      extendType = "noExtend"
      attributePruning	<-FALSE
      postpruning	<-	"none"
      default_rule_pruning	<-	FALSE
      trim_literal_boundaries		<-	FALSE
      defaultRuleOverlapPruning  <- "noPruning"

    }
    else if (qcbaConfID==2)
    {
      attributePruning	<-	TRUE
    }
    else if (qcbaConfID==3)
    {
      trim_literal_boundaries		<-	TRUE
    }
    else if (qcbaConfID==4)
    {
      extendType = "numericOnly"
    }
    else if (qcbaConfID==5)
    {
      postpruning	<-	"cba"
    }
    else if (qcbaConfID==6)
    {
      defaultRuleOverlapPruning  <- c("transactionBased")
    }
    else if (qcbaConfID==7)
    {
      defaultRuleOverlapPruning  <- c("rangeBased")
    }

    for (i in 1:iterations)
    {

      rmQCBA <- qcba(cbaRuleModel=rmCBA,datadf=trainFold,extend=extendType,defaultRuleOverlapPruning=defaultRuleOverlapPruning,attributePruning=attributePruning,trim_literal_boundaries=trim_literal_boundaries,
                     continuousPruning=global_continuousPruning, postpruning=postpruning, fuzzification=global_fuzzification, annotate=global_annotate,minImprovement=minImprovement,
                     minCondImprovement=minCondImprovement,  createHistorySlot=global_createHistorySlot,
                     loglevel = logger)
    }
    end.time <- Sys.time()
    buildTime <- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
    start.time <- Sys.time()
    prediction <- predict(rmQCBA,testFold,testingType=testingType)
    for (i in 1:iterations)  accuracy <- CBARuleModelAccuracy(prediction, testFold[[rmQCBA@classAtt]])
    end.time <- Sys.time()
    predictTime <- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)

    write(c(foldsize,buildTime,predictTime,accuracy, paste("QCBA",qcbaConfID,sep="-")), file = logpath,
          ncolumns = 6,
          append = TRUE, sep = ",")

  }

}


evalQCBA <- function(datasets,experiment_name="testExp",rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE), pruning_options=NULL,extendType="numericOnly",defaultRuleOverlapPruning = "noPruning", attributePruning = FALSE, trim_literal_boundaries = TRUE, continuousPruning=FALSE, postpruning="cba",  fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath=".", minImprovement=0,minCondImprovement=-1,minConf = 0.5,  extensionStrategy="ConfImprovementAgainstLastConfirmedExtension",debug=FALSE, auto=FALSE)
{

  # Write headers for results
  cba_result_file <- paste(basePath,.Platform$file.sep,"result",.Platform$file.sep,experiment_name,"-cba.csv",sep="")
  qcba_result_file <- paste(basePath,.Platform$file.sep,"result",.Platform$file.sep,experiment_name,"-qcba.csv",sep="")

  if (!file.exists(qcba_result_file))
  {
    write(paste("dataset,accuracy,rules,antlength,buildtime"), file = qcba_result_file,
          ncolumns = 1,
          append = FALSE, sep = ",")
  }
  if (!file.exists(cba_result_file))
  {
    write(paste("dataset,accuracy,rules,antlength,buildtime"), file = cba_result_file,
          ncolumns = 1,
          append = FALSE, sep = ",")
  }

  #process datasets
  for (dataset in datasets[1:length(datasets)]) {
    # check if result already computed and skip if so
    qcba_file_text <- readLines(qcba_result_file)
    check_result <- TRUE %in% grepl(paste("^",dataset,",",sep=""),qcba_file_text)
    if (isTRUE(check_result))
    {
      message(paste("Skipping dataset",dataset,"(already computed)"))
      next
    }

    # proceed to computation
    accSumCBA <- 0
    accSumQCBA <- 0
    ruleSumCBA <- 0
    ruleSumQCBA <- 0
    ruleLengthCBA<-0
    ruleLengthQCBA<-0

    buildTimeCBA<-0
    buildTimeQCBA<-0
    maxFoldIndex  <-foldsToProcess -1
    for (fold in 0:maxFoldIndex)
    {
      message(paste("processing:", dataset,fold))
      trainPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".csv", sep="")
      testPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds",.Platform$file.sep,"test",.Platform$file.se,dataset, fold, ".csv", sep="")
      trainFold <- utils::read.csv(trainPath  , header  =TRUE, check.names = FALSE)
      testFold <- utils::read.csv(testPath  , header  =TRUE, check.names = FALSE)
      classAtt <- colnames(trainFold)[length(trainFold)]
      set.seed(111)

      #Run and store results from CBA
      start.time <- Sys.time()
      rmCBA <- cba(trainFold, classAtt=classAtt,rulelearning_options=rulelearning_options,pruning_options=pruning_options)
      buildTimeCBA <- buildTimeCBA + (Sys.time() - start.time)

      #replace sapply(trainFold, class) with rmCBA@origDataTypes
      dataTypes <- rmCBA@attTypes

      rules <- length(rmCBA@rules)
      message(paste("CBA rules:",rules))
      ruleSumCBA <- ruleSumCBA + rules
      prediction <- predict(rmCBA,testFold)
      acc <- CBARuleModelAccuracy(prediction, testFold[[classAtt]])
      message(paste("CBA acc:",acc))
      accSumCBA <- accSumCBA + acc

      avgRuleLengthCBA <- sum(rmCBA@rules@lhs@data)/length(rmCBA@rules)
      ruleLengthCBA<-ruleLengthCBA+avgRuleLengthCBA

      #write debug information
      if (debug==TRUE & auto==FALSE){
        dir.create(file.path(basePath, "debug"), showWarnings = FALSE)
        rulesPath <-paste(basePath,"/debug/",experiment_name,"-",dataset,fold,".arules",sep="")
        write.csv(as(rmCBA@rules,"data.frame"), rulesPath, row.names=TRUE,quote = TRUE)

        x=paste('<!DOCTYPE properties SYSTEM "http://java.sun.com/dtd/properties.dtd">',
                "<properties>\n",
                "<entry key=\"Method\">extend</entry>\n",
                "<entry key=\"RulesPath\">", getwd(), "/",rulesPath, "</entry>\n",
                "<entry key=\"TrainDataPath\">", getwd(),"/", trainPath,"</entry>\n",
                "<entry key=\"ExtendType\">",extendType,"</entry>\n",
                "<entry key=\"Annotate\">",annotate, "</entry>\n",
                "<entry key=\"ContinuousPruning\">",continuousPruning, "</entry>\n",
                "<entry key=\"AttributePruning\">",attributePruning, "</entry>\n",
                "<entry key=\"Trimming\">",trim_literal_boundaries, "</entry>\n",
                "<entry key=\"Fuzzification\">",fuzzification, "</entry>\n",
                "<entry key=\"DefaultRuleOverlapPruning\">",defaultRuleOverlapPruning, "</entry>\n",
                "<entry key=\"Postpruning\">",postpruning, "</entry>\n",
                "<entry key=\"MinCondImprovement\">",minCondImprovement, "</entry>\n",
                "<entry key=\"DataTypes\">", paste(dataTypes, collapse = ','),'</entry>\n',
                "<entry key=\"TargetAttribute\">", classAtt,'</entry>\n',
                "<entry key=\"OutputPath\">", getwd(),"/debug/",experiment_name,"-",dataset,fold,'-qcba.arules</entry>\n',
                "</properties>", sep="")

        qcbaFilePath <-paste(basePath,"/debug/",experiment_name, "-",dataset,fold,".xml",sep="")
        write(x, file = qcbaFilePath,
              ncolumns = 1,
              append = FALSE, sep = ",")
      }
      #Run and store results from QCBA
      start.time <- Sys.time()
      if (auto==FALSE)
      {
        rmQCBA <- qcba(cbaRuleModel=rmCBA,datadf=trainFold,extend=extendType,defaultRuleOverlapPruning=defaultRuleOverlapPruning,attributePruning=attributePruning,trim_literal_boundaries=trim_literal_boundaries,
                       continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=fuzzification, annotate=annotate,minImprovement=minImprovement,
                       minCondImprovement=minCondImprovement,  createHistorySlot=global_createHistorySlot,
                       loglevel = logger)
      }
      else
      {
        rmQCBA <-  evalAutoFitQCBA(rmCBA=rmCBA,trainFold=trainFold, classAtt=classAtt)
      }

      buildTimeQCBA <- buildTimeQCBA + (Sys.time() - start.time)
      #counting commas gives for each rule one less number of literals
      avgRuleLengthQCBA <- (sum(unlist(lapply(rmQCBA@rules[1],str_count,pattern=",")))+
                              # assuming the last rule has antecedent length zero - not counting its length
                              nrow(rmQCBA@rules)-1)/nrow(rmQCBA@rules)
      ruleLengthQCBA<-ruleLengthQCBA+avgRuleLengthQCBA
      prediction <- predict(rmQCBA,testFold,testingType=testingType,loglevel=logger)
      acc <- CBARuleModelAccuracy(prediction, testFold[[rmQCBA@classAtt]])
      message(paste("QCBA acc:", acc))
      accSumQCBA <-  accSumQCBA+ acc
      rules <- rmQCBA@ruleCount
      message(paste("QCBA rules:",rules))
      ruleSumQCBA <- ruleSumQCBA + rules
    }
    accCBA <- accSumCBA/foldsToProcess
    accQCBA <- accSumQCBA/foldsToProcess
    rulesCBA<- ruleSumCBA/foldsToProcess
    rulesQCBA <-ruleSumQCBA/foldsToProcess
    ruleLengthCBA<- ruleLengthCBA/foldsToProcess
    ruleLengthQCBA <-ruleLengthQCBA/foldsToProcess


    #write results
    # write(paste(paste(dataset,accCBA,rulesCBA,accQCBA,rulesQCBA,format(Sys.time(),"%X"),"",sep=";"),paste(as.list(match.call())[-1:-2],collapse=";")), file = detailed_result_file,
    # ncolumns = 1,
    # append = TRUE, sep = ";")
    buildTimeCBA <- round(as.numeric(buildTimeCBA/foldsToProcess,units="secs"),2)
    buildTimeQCBA <- round(as.numeric(buildTimeQCBA/foldsToProcess,units="secs"),2)
    write(c(dataset,accCBA,rulesCBA,ruleLengthCBA,buildTimeCBA), file = cba_result_file,
            ncolumns = 5,
            append = TRUE, sep = ",")
    write(c(dataset,accQCBA,rulesQCBA,ruleLengthQCBA,buildTimeQCBA), file = qcba_result_file,
            ncolumns = 5,
            append = TRUE, sep = ",")
  }
}

doEvalTimeKDD <- function()
{
  logpath <- "result/QCBA-scaling.csv"
  write(c("foldsize","buildTime","predictTime","accuracy", "alg"), file = logpath,
        ncolumns = 6,
        append = TRUE, sep = ",")
  for (foldsize in c(1000,10000,20000,30000,40000,50000,"100000","500000","1000000") )
  {
    if (foldsize < 100000) next
    trainPath <-paste("data/scaling/KDDCup99_",foldsize,".csv",sep="")
    testPath <- "data/scaling/KDDCup99_full_test.csv"
    trainFold <- utils::read.csv(trainPath  , header  =TRUE, check.names = FALSE)
    testFold <- utils::read.csv(testPath  , header  =TRUE, check.names = FALSE)
    iterations<-1
    evalTimeQCBA(trainFold,testFold,foldsize,logpath,iterations=iterations,includeQCBA=TRUE)
  }
}

experimentName <- function(experimentToRun,extendType,default_rule_pruning,attributePruning,trim_literal_boundaries,continuousPruning,postpruning,defaultRuleOverlapPruning,minCondImprovement,minImprovement)
{
  name <- paste(experimentToRun,extendType,sep="-")
  if (default_rule_pruning)
  {
    name <- paste(name,"D",sep="-")
  }
  if (trim_literal_boundaries)
  {
    name <- paste(name,"T",sep="-")
  }
  if (continuousPruning)
  {
    name <- paste(name,"C",sep="-")
  }
  if (postpruning == "cba")
  {
    name <- paste(name,"Pcba",sep="-")
  }
  if (postpruning == "greedy")
  {
    name <- paste(name,"Pgre",sep="-")
  }
  if (attributePruning)
  {
    name <- paste(name,"A",sep="-")
  }

  if (defaultRuleOverlapPruning!="noPruning")
  {
    name <- paste(name,defaultRuleOverlapPruning,sep="-")
  }
  name <- paste(name, "-mci=",minCondImprovement,sep="")

  if (minImprovement!=0)
  {
    name <- paste(name, "-mi=",minImprovement,sep="")
  }
  return(name)
}

NextendType = c("noExtend","numericOnly")
NattributePruning	<-	c(TRUE,FALSE)
NcontinuousPruning	<-	c(TRUE,FALSE)
Npostpruning	<-	c("none","cba","greedy")
Ndefault_rule_pruning	<-	c(TRUE,FALSE)
Ntrim_literal_boundaries		<-	c(TRUE,FALSE)
NdefaultRuleOverlapPruning  <- c("transactionBased","rangeBased","noPruning")
rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE)

if (is.null(experimentToRun))
{
  message("\n Pass experiment number as argument to run it.")
} else if (experimentToRun==-1)
{
  message("hyperparameter search with default CBA settings (rulelearning_options)")
  evalQCBA(datasets=datasets,experiment_name="hyper1",rulelearning_options=rulelearning_options,auto=TRUE,basePath=".",debug=debug)
} else if (experimentToRun==-2)
{
  message("hyperparameter search with extended CBA settings (rulelearning_options)")
  rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=50, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE)
  evalQCBA(datasets=datasets,experiment_name="hyper2",rulelearning_options=rulelearning_options,auto=TRUE,basePath=".",debug=debug)
} else if (experimentToRun>1)
{   
  message("Running a specific experiment")
  combination_no <- 1 #max 196
  for (extendType in NextendType){
    for (trim_literal_boundaries in Ntrim_literal_boundaries) {
      for (continuousPruning in NcontinuousPruning ) {
        for (postpruning in Npostpruning ) {
          for (attributePruning in  NattributePruning) {
            for (default_rule_pruning in Ndefault_rule_pruning) {
              for (defaultRuleOverlapPruning in NdefaultRuleOverlapPruning) {
                  experiment_name<-experimentName(combination_no,extendType,default_rule_pruning,attributePruning,trim_literal_boundaries,continuousPruning,postpruning,defaultRuleOverlapPruning,minCondImprovement,minImprovement)
                  message(experiment_name)
                  if (!onlyList & experimentToRun==combination_no)
                  {
                    evalQCBA(datasets=datasets,rulelearning_options=rulelearning_options,extendType=extendType,experiment_name=experiment_name,
                             pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),
                             attributePruning=attributePruning,trim_literal_boundaries=trim_literal_boundaries,minCondImprovement=minCondImprovement,minImprovement=minImprovement,
                             defaultRuleOverlapPruning=defaultRuleOverlapPruning,continuousPruning=continuousPruning,
                             postpruning=postpruning,basePath=".",debug=debug)
                  }
                combination_no<-combination_no+1
              }
            }
          }
        }
      }
    }
  }
}


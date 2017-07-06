library(qCBA)

logger<-"WARNING"
global_createHistorySlot<-FALSE
evalTimeQCBA <- function(trainFold,testFold,foldsize,logpath,iterations,includeQCBA)
{
  classAtt <- colnames(trainFold)[length(trainFold)]
  start.time <- Sys.time()
  for (i in 1:iterations)  rmCBA <- cba(trainFold, classAtt=classAtt,rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE),
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

  start.time <- Sys.time()
  for (i in 1:iterations)
    {
    .jinit(force.init = TRUE,parameters="-Xmx4g")
    rmQCBA <- qcba(cbaRuleModel=rmCBA,datadf=trainFold,loglevel = logger)
  }
  end.time <- Sys.time()
  buildTime <- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)

  start.time <- Sys.time()
  prediction <- predict(rmQCBA,testFold,testingType=testingType)
  for (i in 1:iterations)  accuracy <- CBARuleModelAccuracy(prediction, testFold[[rmQCBA@classAtt]])
  end.time <- Sys.time()
  predictTime <- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)

  write(c(foldsize,buildTime,predictTime,accuracy, "QCBA"), file = logpath,
        ncolumns = 6,
        append = TRUE, sep = ",")
}

evalQCBA <- function(datasets,experiment_name="testExp",rulelearning_options=NULL, pruning_options=NULL,trim_literal_boundaries = TRUE, continuousPruning=FALSE, postpruning=FALSE, fuzzification=FALSE, annotate=FALSE,testingType="mixture",basePath=".", minImprovement=0,minCondImprovement=-1,minConf = 0.5,  extensionStrategy="ConfImprovementAgainstLastConfirmedExtension",debug=FALSE)
{
  
  # Write headers for results
  detailed_result_file <- paste(basePath,.Platform$file.sep,"result",.Platform$file.sep,"summary.csv",sep="")
  cba_result_file <- paste(basePath,.Platform$file.sep,"result",.Platform$file.sep,experiment_name,"-cba.csv",sep="")
  qcba_result_file <- paste(basePath,.Platform$file.sep,"result",.Platform$file.sep,experiment_name,"-qcba.csv",sep="")
  
  if (!file.exists(qcba_result_file))
  {
    write(paste("dataset,accuracy,rules"), file = qcba_result_file,
          ncolumns = 1,
          append = FALSE, sep = ",")
    return();
  }
  if (!file.exists(cba_result_file))
  {
    write(paste("dataset,accuracy,rules"), file = cba_result_file,
          ncolumns = 1,
          append = FALSE, sep = ",")
  }
  
  write(paste("dataset;accCBA;rulesCBA;accQCBA;rulesQCBA;timestamp;",paste(names(as.list(match.call()))[-1:-2],collapse=";")), file = detailed_result_file,
          ncolumns = 1,
          append = FALSE, sep = ",")

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
    for (fold in 0:9)
    {
      message(paste("processing:", dataset,fold))
      trainPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".csv", sep="")
      testPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds",.Platform$file.sep,"test",.Platform$file.se,dataset, fold, ".csv", sep="")
      trainFold <- utils::read.csv(trainPath  , header  =TRUE, check.names = FALSE)
      testFold <- utils::read.csv(testPath  , header  =TRUE, check.names = FALSE)
      classAtt <- colnames(trainFold)[length(trainFold)]
      set.seed(111)

      #Run and store results from CBA
      rmCBA <- cba(trainFold, classAtt=classAtt,rulelearning_options=rulelearning_options,pruning_options=pruning_options)
      #replace sapply(trainFold, class) with rmCBA@origDataTypes
      dataTypes <- rmCBA@attTypes

      rules <- length(rmCBA@rules)
      message(paste("CBA rules:",rules))
      ruleSumCBA <- ruleSumCBA + rules
      prediction <- predict(rmCBA,testFold)
      acc <- CBARuleModelAccuracy(prediction, testFold[[classAtt]])
      message(paste("CBA acc:",acc))
      accSumCBA <- accSumCBA + acc

      rulesPath <-paste(basePath,"result/",dataset,fold,".arules",sep="")

      #write debug information
      if (debug){
        write.csv(as(rmCBA@rules,"data.frame"), rulesPath, row.names=TRUE,quote = TRUE)


        x=paste('<!DOCTYPE properties SYSTEM "http://java.sun.com/dtd/properties.dtd">',
                "<properties>",
                "<entry key=\"Method\">extend</entry>",
                "<entry key=\"RulesPath\">", getwd(), "/",rulesPath, "</entry>",
                "<entry key=\"TrainDataPath\">", getwd(),"/", trainPath,"</entry>",
                "<entry key=\"ExtendRuleSortComparator\">MMACRuleComparator</entry>",
                "<entry key=\"ExtendType\">numericOnly</entry>",
                "<entry key=\"PruneAfterExtend\">True</entry>",
                '<entry key="ContinuousPruning">False</entry>',
                '<entry key="Fuzzification">False</entry>',
                '<entry key="Annotate">False</entry>',
                '<entry key="DataTypes">', paste(dataTypes, collapse = ','),'</entry>',
                '<entry key="TargetAttribute">', classAtt,'</entry>',
                '<entry key="OutputPath">',dataset,fold,'-qcba.arules</entry>',
                "</properties>", sep="")

        qcbaFilePath <-paste(basePath,"result/",dataset,fold,".xml",sep="")
        write(x, file = qcbaFilePath,
              ncolumns = 1,
              append = FALSE, sep = ",")
      }

      #Run and store results from QCBA
      rmQCBA <- qcba(cbaRuleModel=rmCBA,datadf=trainFold,trim_literal_boundaries=trim_literal_boundaries,continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=fuzzification, annotate=annotate,
                           minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,  extensionStrategy=extensionStrategy,createHistorySlot=global_createHistorySlot,
                           loglevel = logger)
      prediction <- predict(rmQCBA,testFold,testingType=testingType,loglevel=logger)
      acc <- CBARuleModelAccuracy(prediction, testFold[[rmQCBA@classAtt]])
      message(paste("QCBA acc:", acc))
      accSumQCBA <-  accSumQCBA+ acc
      rules <- rmQCBA@ruleCount
      message(paste("QCBA rules:",rules))
      ruleSumQCBA <- ruleSumQCBA + rules
    }
    accCBA <- accSumCBA/10
    accQCBA <- accSumQCBA/10
    rulesCBA<- ruleSumCBA/10
    rulesQCBA <-ruleSumQCBA/10

    #write results
    write(paste(paste(dataset,accCBA,rulesCBA,accQCBA,rulesQCBA,format(Sys.time(),"%X"),"",sep=";"),paste(as.list(match.call())[-1:-2],collapse=";")), file = detailed_result_file,
    ncolumns = 1,
    append = TRUE, sep = ";")
    write(c(dataset,accCBA,rulesCBA), file = cba_result_file,
            ncolumns = 3,
            append = TRUE, sep = ",")
    write(c(dataset,accQCBA,rulesQCBA), file = qcba_result_file,
            ncolumns = 3,
            append = TRUE, sep = ",")
  }
}

doEvalTime <- function()
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

#doEvalTime()

datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
continuousPruning	<-	FALSE
postpruning	<- FALSE
default_rule_pruning	<- FALSE
trim_literal_boundaries	<-	FALSE
maxtime=1000
target_rule_count=50000
minImprovement=0
minCondImprovement=-1
minConf = 0.5
extensionStrategy="ConfImprovementAgainstLastConfirmedExtension" # ConfImprovementAgainstLastConfirmedExtension, MinConf

args <- commandArgs(trailingOnly = TRUE)
if (is.null(args))
{
  args = c(70)
}
experimentToRun=args[1]


if (experimentToRun==1)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"OnlyExtend",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

default_rule_pruning	<- TRUE
if (experimentToRun==2)
{
evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"DR",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
         pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
         continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
trim_literal_boundaries	<- TRUE
if (experimentToRun==3)
{
evalQCBA(datasets=datasets,paste(experimentToRun,"DR_TR_",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
         pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
         continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
postpruning <- TRUE
if (experimentToRun==4)
{
evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"DR_TR_P",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
         pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
         continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
continuousPruning <- TRUE
if (experimentToRun==5)
{
evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"DR_TR_P_C",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
         pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
         continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
continuousPruning <- FALSE
postpruning <- FALSE
default_rule_pruning	<- FALSE
trim_literal_boundaries	<- TRUE
if (experimentToRun==6)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
postpruning <- TRUE
if (experimentToRun==7)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR_P",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

if (experimentToRun==70)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR_P-updatedDef",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

continuousPruning <- TRUE
if (experimentToRun==8)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR_P_C",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

minCondImprovement <- -0.05

if (experimentToRun==9)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR_P_C_mci-0.05",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
minCondImprovement<- -1
continuousPruning <- FALSE
postpruning <- FALSE
default_rule_pruning	<- FALSE
trim_literal_boundaries	<- FALSE
minImprovement <-0

#BEST - same accuracy as CBA (0.81, 20% less rules)
postpruning <- TRUE
if (experimentToRun==10)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"P",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
continuousPruning <- TRUE
if (experimentToRun==11)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"P_C",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

minCondImprovement <- -0.05

if (experimentToRun==12)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"C_mci-0.05",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
continuousPruning	<-	FALSE
postpruning	<- FALSE
default_rule_pruning	<- FALSE
trim_literal_boundaries	<-	FALSE
maxtime=1000
target_rule_count=50000
minImprovement=0
minCondImprovement=-1
minConf = 0.5
extensionStrategy="ConfImprovementAgainstLastConfirmedExtension"


if (experimentToRun==13)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"OnlyExtend-CIALCE",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

trim_literal_boundaries<-TRUE
if (experimentToRun==14)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR-CIALCE",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

postpruning <- TRUE
if (experimentToRun==15)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR-P-CIALCE",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
continuousPruning <- TRUE
if (experimentToRun==16)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR-PC-CIALCE",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

trim_literal_boundaries<-FALSE
postpruning <- TRUE
if (experimentToRun==17)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"P-CIALCE",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
continuousPruning <- TRUE
if (experimentToRun==18)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"PC-CIALCE",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

continuousPruning	<-	FALSE
postpruning	<- FALSE
default_rule_pruning	<- FALSE
trim_literal_boundaries	<-	FALSE
maxtime=1000
target_rule_count=80000
minImprovement=0
minCondImprovement=-1
minConf = 0.5
extensionStrategy="ConfImprovementAgainstLastConfirmedExtension" # ConfImprovementAgainstLastConfirmedExtension, MinConf

trim_literal_boundaries	<-	TRUE
postpruning <- TRUE
if (experimentToRun==19)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"P80k",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}
target_rule_count=50000
extensionStrategy <-"ConfImprovementAgainstSeedRule"
if (experimentToRun==20)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR-P-SR",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}

target_rule_count=50000
extensionStrategy <-"MinConf"
if (experimentToRun==21)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR-P-MinConf",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}



continuousPruning	<-	FALSE
postpruning	<- TRUE
default_rule_pruning	<- FALSE
trim_literal_boundaries	<-	TRUE
maxtime=1000
target_rule_count=50000
minImprovement=0.05
minCondImprovement=-1
minConf = 0.5
extensionStrategy="ConfImprovementAgainstLastConfirmedExtension" # ConfImprovementAgainstLastConfirmedExtension, MinConf


if (experimentToRun==22)
{
  evalQCBA(datasets=datasets,experiment_name=paste(experimentToRun,"TR_P_mi0.05",sep = "-"),rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=target_rule_count, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=default_rule_pruning, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=trim_literal_boundaries,minImprovement=minImprovement,minCondImprovement=minCondImprovement,minConf = minConf,
           continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=FALSE, annotate=FALSE,extensionStrategy=extensionStrategy,testingType="oneRule",basePath=".")
}


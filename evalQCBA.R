library(qCBA)

logger<-"WARNING"
global_createHistorySlot<-FALSE
evalTimeQCBA <- function(trainFold,testFold,foldsize,logpath,iterations,includeQCBA)
{
  classAtt <- colnames(trainFold)[length(trainFold)]
  start.time <- Sys.time()
  for (i in 1:iterations)  rmCBA <- cba(trainFold, classAtt=classAtt)
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

  write(paste("dataset,accuracy,rules"), file = cba_result_file,
        ncolumns = 1,
        append = FALSE, sep = ",")
  write(paste("dataset,accuracy,rules"), file = qcba_result_file,
          ncolumns = 1,
          append = FALSE, sep = ",")
  write(paste("dataset;accCBA;rulesCBA;accQCBA;rulesQCBA;timestamp;",paste(names(as.list(match.call()))[-1:-2],collapse=";")), file = detailed_result_file,
          ncolumns = 1,
          append = FALSE, sep = ",")

  #process datasets
  for (dataset in datasets[1:length(datasets)]) {
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

doEvalAccDetailed <- function()
{
  resultsFile="result/qcba.csv"
  target_rule_count<-10000
  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = target_rule_count, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=TRUE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),
           continuousPruning=TRUE, postpruning=TRUE, fuzzification=TRUE, annotate=TRUE,testingType="mixture",basePath="",resultsFile=resultsFile)
}

doEvalAccQCBA_TP_ConfImprovementAgainstSeedRule <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE), trim_literal_boundaries=TRUE,
           continuousPruning=FALSE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",extensionStrategy="ConfImprovementAgainstSeedRule",basePath="",resultsFile="",oneConfigResultFile="result/TP_ConfImprovementAgainstSeedRule.csv",cbaResultFile= "result/CBA-accuracy.csv")
}

doEvalAccQCBA_TCP_ConfImprovementAgainstSeedRule_nodefaultCBApruning <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=FALSE),
           pruning_options=list(default_rule_pruning=FALSE, rule_window=100,greedy_pruning=FALSE), trim_literal_boundaries=TRUE,
           continuousPruning=TRUE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",
           extensionStrategy="ConfImprovementAgainstSeedRule",basePath="",resultsFile="",oneConfigResultFile="result/TCP_ConfImprovementAgainstSeedRule.csv",cbaResultFile= "result/CBA-accuracy.csv",
           minImprovement=0,minCondImprovement=-1,minConf = 0.5)
}

doEvalAccQCBA_TP_ConfImprovementAgainstSeedRule <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE), trim_literal_boundaries=TRUE,
           continuousPruning=FALSE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",
           extensionStrategy="ConfImprovementAgainstSeedRule",basePath="",resultsFile="",oneConfigResultFile="result/TP_ConfImprovementAgainstSeedRule.csv",cbaResultFile= "result/CBA-accuracy.csv")
}

doEvalAccQCBA_T_ConfImprovementAgainstSeedRule <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE), trim_literal_boundaries=TRUE,
           continuousPruning=FALSE, postpruning=FALSE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",
           extensionStrategy="ConfImprovementAgainstSeedRule",basePath="",resultsFile="",oneConfigResultFile="result/T_ConfImprovementAgainstSeedRule.csv",cbaResultFile= "result/CBA-accuracy.csv")
}


doEvalAccQCBA_C_ConfImprovementAgainstSeedRule_Notrim_literal_boundaries <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE), trim_literal_boundaries=FALSE,
           continuousPruning=TRUE, postpruning=FALSE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",extensionStrategy="ConfImprovementAgainstSeedRule",basePath="",resultsFile="",oneConfigResultFile="result/QCBA-C-accuracy-ConfImprovementAgainstSeedRuleNotrim_literal_boundaries.csv",cbaResultFile= "result/CBA-accuracy.csv")
}

doEvalAccQCBA_C_MinConf <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE), trim_literal_boundaries=TRUE,
           continuousPruning=TRUE, postpruning=FALSE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",extensionStrategy="MinConf",basePath="",resultsFile="",oneConfigResultFile="result/QCBA-C-MinConf.csv",cbaResultFile= "result/CBA-accuracy.csv")
}


doEvalAccQCBA_C_ConfImprovementAgainstLastConfirmedExtension <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE), trim_literal_boundaries=TRUE,
           continuousPruning=TRUE, postpruning=FALSE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",extensionStrategy="ConfImprovementAgainstLastConfirmedExtension",basePath="",resultsFile="",oneConfigResultFile="result/QCBA-C-accuracy.csv",cbaResultFile= "result/CBA-accuracy.csv")
}

doEvalAccQCBA_P <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=TRUE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),
           continuousPruning=FALSE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath="",resultsFile="",oneConfigResultFile="result/QCBA-P-accuracy.csv",cbaResultFile= "result/CBA-accuracy.csv")
}

doEvalAccQCBA_P_nodefaultpruning <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=TRUE),
           pruning_options=list(default_rule_pruning=FALSE, rule_window=100,greedy_pruning=FALSE),
           continuousPruning=FALSE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath="",resultsFile="",oneConfigResultFile="result/QCBA-P-Nodef-accuracy.csv",cbaResultFile= "result/CBA-Nodef-accuracy.csv")
}

doEvalAccQCBA_CP <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=TRUE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),
           continuousPruning=TRUE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath="",resultsFile="",oneConfigResultFile="result/QCBA-CP-accuracy.csv",cbaResultFile= "result/CBA-accuracy.csv")
}


doEvalAccQCBA_manual <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),
           continuousPruning=FALSE, postpruning=FALSE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath="",resultsFile="",oneConfigResultFile="result/manual-accuracy.csv",cbaResultFile= "result/CBA-accuracy.csv")
}

doEvalAccQCBA_manual_TCP <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=TRUE,
           continuousPruning=TRUE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath="",resultsFile="",oneConfigResultFile="result/manual-TCP-accuracy_x.csv",cbaResultFile= "result/CBA-accuracy_x
          .csv")
}

doEvalAccQCBA_manual_TP<- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=TRUE,minImprovement=0,minCondImprovement=-1,minConf = 0.5,
           continuousPruning=FALSE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath="",resultsFile="",oneConfigResultFile="result/manual_TP.csv",cbaResultFile= "result/CBA-accuracy.csv")
}



doEvalAccQCBA_manual_P<- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=FALSE,minImprovement=0,minCondImprovement=-1,minConf = 0.5,
           continuousPruning=FALSE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath="",resultsFile="",oneConfigResultFile="result/manual_P.csv",cbaResultFile= "result/CBA-accuracy.csv")
}


doEvalAccQCBAMultiRule <- function()
{

  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=TRUE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),
           continuousPruning=FALSE, postpruning=TRUE, fuzzification=FALSE, annotate=TRUE,testingType="mixture",basePath="",resultsFile="",oneConfigResultFile="result/QCBA-MultiRule-accuracy.csv",cbaResultFile= "")
}

doEvalTime <- function()
{
  logpath <- "result/QCBA-scaling.csv"
  write(c("foldsize","buildTime","predictTime","accuracy", "alg"), file = logpath,
        ncolumns = 6,
        append = TRUE, sep = ",")
  for (foldsize in c(1000,10000,20000,30000,40000,50000) )
  {
    if (foldsize < 50000) next
    trainPath <-paste("data/scaling/KDDCup99_",foldsize,".csv",sep="")
    testPath <- "data/scaling/KDDCup99_full_test.csv"
    trainFold <- utils::read.csv(trainPath  , header  =TRUE, check.names = FALSE)
    testFold <- utils::read.csv(testPath  , header  =TRUE, check.names = FALSE)
    iterations<-1
    evalTimeQCBA(trainFold,testFold,foldsize,logpath,iterations=iterations,includeQCBA=TRUE)
  }
}


doEvalAccQCBA_test<- function()
{
  setwd("/home/tomas/temp/arcBench")
  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalQCBA(datasets=datasets,experiment_name="testExp",rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=1000, trim=TRUE, find_conf_supp_thresholds=FALSE),
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),trim_literal_boundaries=TRUE,minImprovement=0,minCondImprovement=-1,minConf = 0.5,
           continuousPruning=FALSE, postpruning=TRUE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath=".")
}
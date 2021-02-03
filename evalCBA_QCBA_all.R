library(stringr)
library(arc)
library(qCBA)
args <- commandArgs(trailingOnly = TRUE)
print(args)
length(args)
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
} else if (length(args)==3|length(args)==5) {
  minCondImprovement = as.double(args[2])
  minImprovement = as.double(args[3])
}
message(paste("Using minCondImprovement", minCondImprovement))
message(paste("Using minImprovement", minImprovement))

experimentToRun=args[1]
#datasets <- c("labor")
output_fname_tag <- ""
if (length(args)==5)
{
  warning("overriding datasets")
  datasets <- unlist(strsplit(args[4], split = ";"))
  output_fname_tag <- args[5]
  message(paste("tag",output_fname_tag))
}
#datasets with binary class
#output_fname_tag <- "exp"
#datasets <- c("hepatitis","ionosphere","sonar","spambase","australian","breast-w","colic","credit-a","diabetes","heart-statlog","credit-g")
#experimentToRun <-c(117)


foldsToProcess <-10

logger<-"WARNING"
global_createHistorySlot<-FALSE

#common settings
global_maxtime <- 1000
global_target_rule_count=50000
global_fuzzification <- FALSE
global_annotate <- FALSE
global_testingType <- "oneRule"
global_minConf = 0.5
global_extensionStrategy="ConfImprovementAgainstLastConfirmedExtension"
global_continuousPruning	<- FALSE

evalQCBA <- function(datasets,experiment_name="testExp",rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE), pruning_options=NULL,extendType="numericOnly",defaultRuleOverlapPruning = "noPruning", attributePruning = FALSE, trim_literal_boundaries = TRUE, continuousPruning=FALSE, postpruning="cba",  fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath=".", minImprovement=0,minCondImprovement=-1,minConf = 0.5,  extensionStrategy="ConfImprovementAgainstLastConfirmedExtension",debug=FALSE, auto=FALSE)
{

  # Write headers for results
  cba_result_file <- paste(basePath,.Platform$file.sep,"CBA_results",.Platform$file.sep,experiment_name,"-cba",output_fname_tag,".csv",sep="")
  qcba_result_file <- paste(basePath,.Platform$file.sep,"CBA_results",.Platform$file.sep,experiment_name,"-qcba",output_fname_tag,".csv",sep="")

  if (!file.exists(qcba_result_file))
  {
    write(paste("dataset,accuracy,rules,antlength,buildtime,auc"), file = qcba_result_file,
          ncolumns = 1,
          append = FALSE, sep = ",")
  }
  if (!file.exists(cba_result_file))
  {
    write(paste("dataset,accuracy,rules,antlength,buildtime,auc"), file = cba_result_file,
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
    aucSumQCBA <-0
    aucSumCBA <-0
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
      trainPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".csv", sep="")
      testPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"test",.Platform$file.se,dataset, fold, ".csv", sep="")
      trainFold <- utils::read.csv(trainPath  , header  =TRUE, check.names = FALSE)
      testFold <- utils::read.csv(testPath  , header  =TRUE, check.names = FALSE)
      classAtt <- colnames(trainFold)[length(trainFold)]
      set.seed(111)
      if(!is.factor(trainFold[[classAtt]]))
      {
        trainFold[[classAtt]] <- as.factor(trainFold[[classAtt]])
      }
      if(!is.factor(testFold[[classAtt]]))
      {
        testFold[[classAtt]] <- as.factor(testFold[[classAtt]])
      }
      #Run and store results from CBA
      start.time <- Sys.time()
      rmCBA <- cba(trainFold, classAtt=classAtt,rulelearning_options=rulelearning_options,pruning_options=pruning_options)
      buildTimeCBA <- buildTimeCBA + (Sys.time() - start.time)

      #replace sapply(trainFold, class) with rmCBA@origDataTypes
      dataTypes <- rmCBA@attTypes

      rules <- length(rmCBA@rules)
      write(rmCBA@rules, file = paste0("CBA_models/",dataset,fold,"-",experiment_name,".csv", sep = ""))
      message(paste("CBA rules:",rules))
      ruleSumCBA <- ruleSumCBA + rules
      prediction <- predict(rmCBA,testFold)
      acc <- CBARuleModelAccuracy(prediction, testFold[[classAtt]])
      message(paste("CBA acc:",acc))
      #NEW ROC
      auc<-0
      if (length(levels(trainFold[[classAtt]])) == 2)
      {
        positiveClass<-levels(testFold[[classAtt]])[2]
        confidences <- predict(rmCBA,testFold,outputConfidenceScores=TRUE,positiveClass=positiveClass)
        target<-droplevels(factor(testFold[[classAtt]],ordered = TRUE,levels=levels(testFold[[classAtt]])))
        pred <- ROCR::prediction(confidences, target)
        roc <- ROCR::performance(pred, "tpr", "fpr")
        auc <- ROCR::performance(pred, "auc")
        auc <- unlist(auc@y.values)
        message(paste("CBA auc:",auc))
      }
      aucSumCBA <- aucSumCBA + auc

      # END NEW


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
      write.csv(rmQCBA@rules, file = paste0("QCBA_models/",dataset,fold,"-",experiment_name,".csv", sep = "") )
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
      #NEW ROC
      auc_qcba_fold<-0
      if (length(levels(trainFold[[classAtt]]))== 2)
      {
        positiveClass_qcba<-levels(testFold[[classAtt]])[2]
        confidences_qcba <- predict(rmQCBA,testFold,testingType=testingType,loglevel=logger,outputConfidenceScores=TRUE,positiveClass=positiveClass)
        target_qcba<-droplevels(factor(testFold[[classAtt]],ordered = TRUE,levels=levels(testFold[[classAtt]])))
        pred_qcba <- ROCR::prediction(confidences_qcba, target_qcba)
        roc_qcba <- ROCR::performance(pred_qcba, "tpr", "fpr")
        plot=FALSE
        if (plot)
        {
          plot(roc_qcba, lwd=2, colorize=TRUE)
          lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)
        }
        auc_qcba_fold <- ROCR::performance(pred_qcba, "auc")
        auc_qcba_fold <- unlist(auc_qcba_fold@y.values)
        message(paste("QCBA auc:",auc_qcba_fold))
      }
      aucSumQCBA <- aucSumQCBA + auc_qcba_fold

      # END NEW
    }
    aucQCBA <-aucSumQCBA/foldsToProcess
    aucCBA <- aucSumCBA/foldsToProcess
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
    write(c(dataset,accCBA,rulesCBA,ruleLengthCBA,buildTimeCBA,aucCBA), file = cba_result_file,
            ncolumns = 6,
            append = TRUE, sep = ",")
    write(c(dataset,accQCBA,rulesQCBA,ruleLengthQCBA,buildTimeQCBA,aucQCBA), file = qcba_result_file,
            ncolumns = 6,
            append = TRUE, sep = ",")
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
  message("hyperparameter search with extended CBA settings (maxlen=10)")
  rulelearning_options=list(minsupp=0.01, minconf=0.5, minlen=1, maxlen=10, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE)
  evalQCBA(datasets=datasets,experiment_name="hyper2maxlen=50",rulelearning_options=rulelearning_options,auto=TRUE,basePath=".",debug=debug)
} else if (experimentToRun==-3) {
  message("hyperparameter search with extended CBA settings (minconf=0.1)")
  rulelearning_options=list(minsupp=0.01, minconf=0.1, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE)
  evalQCBA(datasets=datasets,experiment_name="hyper3minconf=0.1",rulelearning_options=rulelearning_options,auto=TRUE,basePath=".",debug=debug)
}  else if (experimentToRun==-4)
{
  message("hyperparameter search with extended CBA settings (minsupp=0.001)")
  rulelearning_options=list(minsupp=0.001, minconf=0.5, minlen=1, maxlen=5, maxtime=1000, target_rule_count=50000, trim=TRUE, find_conf_supp_thresholds=FALSE)
  evalQCBA(datasets=datasets,experiment_name="hyper4minsupp=0.001",rulelearning_options=rulelearning_options,auto=TRUE,basePath=".",debug=debug)
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


library(qCBA)
library(dplyr)
datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")

appendToList <- function(list1,list2){
  # even if length==0, the for cycle would be run once without this condition
  if (length(list2) == 0) return(list1)
  for (i in 1:length(list2))
  {
    list1[[names(list2)[i]]] <- list2[[i]]
  }
  return(list1)
}

evalQCBA <- function(datasets,experiment_name="testExp",rulelearning_options=list(minlen=1, target_rule_count=600, trim=TRUE, find_conf_supp_thresholds=TRUE), pruning_options=NULL,extendType="numericOnly",defaultRuleOverlapPruning = "noPruning", attributePruning = FALSE, trim_literal_boundaries = TRUE, continuousPruning=FALSE, postpruning="cba",  fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath=".", minImprovement=0,minCondImprovement=-1,minConf = 0.5,  extensionStrategy="ConfImprovementAgainstLastConfirmedExtension")
{
  
  # Write headers for results
  cba_result_file <- paste(basePath,.Platform$file.sep,"result",.Platform$file.sep,experiment_name,"-cba.csv",sep="")
  message(cba_result_file)
  
  if (!file.exists(cba_result_file))
  {
    write(paste("dataset,accuracy,rules,antlength,buildtime_rg,buildtime"), file = cba_result_file,
          ncolumns = 1,
          append = FALSE, sep = ",")
  }
  
  #process datasets
  for (dataset in datasets[1:length(datasets)]) {
    # check if result already computed and skip if so
    cba_result <- readLines(cba_result_file)
    check_result <- TRUE %in% grepl(paste("^",dataset,",",sep=""),cba_result)
    if (isTRUE(check_result))
    {
      message(paste("Skipping dataset",dataset,"(already computed)"))
      next
    }
    
    # proceed to computation
    accSumCBA <- 0
    ruleSumCBA <- 0
    ruleLengthCBA<-0

    buildTimeCBA<-0
    buildTimeCBA_rg<-0
    maxFoldIndex  <-foldsToProcess -1
    for (fold in 0:maxFoldIndex)
    {
      message(paste("processing:", dataset,fold))
      trainPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_discr",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".csv", sep="")
      testPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_discr",.Platform$file.sep,"test",.Platform$file.se,dataset, fold, ".csv", sep="")
      trainFold <- utils::read.csv(trainPath  , header  =TRUE, check.names = FALSE)
      testFold <- utils::read.csv(testPath  , header  =TRUE, check.names = FALSE)
      classAtt <- colnames(trainFold)[length(trainFold)]
      set.seed(111)
      
      #Run and store results from CBA
      start.time <- Sys.time()
      
      #VERSION WITH SEPARATE RULE LEARNING AND PRUNING
      appearance <- getAppearance(trainFold, classAtt)
      #turn all fields to factors
      trainFold <- trainFold %>% mutate_if(is.numeric,as.factor)
      
      txns <- as(trainFold,"transactions")
      #rules <- topRules(trainFold, appearance = appearance, target_rule_count = 100, init_support = 0.5,init_conf = 0.9, minlen = 1, init_maxlen = 10)
      
      if("find_conf_supp_thresholds" %in% names(rulelearning_options)) rulelearning_options <- rulelearning_options[ - which(names(rulelearning_options) == "find_conf_supp_thresholds")]
      
      rules <- do.call("topRules", appendToList(list(txns = txns, appearance = appearance), rulelearning_options))
      end.time_rg <- Sys.time()
      rmCBA <- cba_manual(trainFold,rules, txns, appearance$rhs,
                          classAtt, cutp= list(), pruning_options=NULL)
      
      
      #VERSION WITH  RULE LEARNING AND PRUNING IN ONE STEP
      #rmCBA <- cba(trainFold, classAtt=classAtt,rulelearning_options=rulelearning_options,pruning_options=pruning_options)
      buildTimeCBA_rg <- buildTimeCBA_rg + (end.time_rg - start.time)
      buildTimeCBA <- buildTimeCBA + (Sys.time() - start.time)
      
      #replace sapply(trainFold, class) with rmCBA@origDataTypes
      dataTypes <- rmCBA@attTypes
      
      rules <- length(rmCBA@rules)
      message(paste("CBA rules:",rules))
      ruleSumCBA <- ruleSumCBA + rules
      testFold <- testFold %>% mutate_if(is.numeric,as.factor)
      prediction <- predict(rmCBA,testFold)
      acc <- CBARuleModelAccuracy(prediction, testFold[[classAtt]])
      message(paste("CBA acc:",acc))
      accSumCBA <- accSumCBA + acc
      
      avgRuleLengthCBA <- sum(rmCBA@rules@lhs@data)/length(rmCBA@rules)
      ruleLengthCBA<-ruleLengthCBA+avgRuleLengthCBA
      
    }
    accCBA <- accSumCBA/foldsToProcess
    rulesCBA<- ruleSumCBA/foldsToProcess
    ruleLengthCBA<- ruleLengthCBA/foldsToProcess

    
    #write results
    # write(paste(paste(dataset,accCBA,rulesCBA,accQCBA,rulesQCBA,format(Sys.time(),"%X"),"",sep=";"),paste(as.list(match.call())[-1:-2],collapse=";")), file = detailed_result_file,
    # ncolumns = 1,
    # append = TRUE, sep = ";")
    buildTimeCBA <- round(as.numeric(buildTimeCBA/foldsToProcess,units="secs"),2)
    buildTimeCBA_rg <- round(as.numeric(buildTimeCBA_rg/foldsToProcess,units="secs"),2)
    write(c(dataset,accCBA,rulesCBA,ruleLengthCBA,buildTimeCBA_rg,buildTimeCBA), file = cba_result_file,
          ncolumns = 6,
          append = TRUE, sep = ",")
  }
}

evalQCBA(datasets=datasets,experiment_name="hyper1",basePath=".")
library(rMARC)

evalTimeMARC <- function(trainFold,testFold,foldsize,logpath,iterations,includeMARC)
{
  
  logger<-"WARNING"
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
  
  if (!includeMARC) return()
  
  start.time <- Sys.time()
  for (i in 1:iterations) 
    {
    .jinit(force.init = TRUE,parameters="-Xmx4g")
    rmMARC <- marcExtend(cbaRuleModel=rmCBA,datadf=trainFold,loglevel = logger)
  }
  end.time <- Sys.time()
  buildTime <- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
  
  start.time <- Sys.time()
  prediction <- predict(rmMARC,testFold,testingType=testingType)
  for (i in 1:iterations)  accuracy <- CBARuleModelAccuracy(prediction, testFold[[rmMARC@classAtt]])
  end.time <- Sys.time()
  predictTime <- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
  
  
  write(c(foldsize,buildTime,predictTime,accuracy, "MARC"), file = logpath,
        ncolumns = 6,
        append = TRUE, sep = ",")  
}

evalMARC <- function(datasets,rulelearning_options=NULL, pruning_options=NULL,continuousPruning=FALSE, postpruning=FALSE, fuzzification=FALSE, annotate=FALSE,testingType="mixture", resultsFile="",basePath="",oneConfigResultFile="", cbaResultFile="", debug=FALSE)
{
  if (oneConfigResultFile!="")
  {
    write(paste("dataset,accuracy,rules"), file = oneConfigResultFile,
          ncolumns = 1,
          append = FALSE, sep = ",")
  }
  if (cbaResultFile!="")
  {
    write(paste("dataset,accuracy,rules"), file = cbaResultFile,
          ncolumns = 1,
          append = FALSE, sep = ",")
  }  
  logger<-"WARNING"
  
  for (dataset in datasets[1:length(datasets)]) {
    accSumCBA<- 0
    accSumMARC<- 0
    ruleSumCBA <- 0
    ruleSumMARC <- 0
    for (fold in 0:9)
    {
      message(paste("processing:", dataset,fold))
      trainPath <- paste(basePath,"data/folds/train/",dataset, fold, ".csv", sep="")
      testPath <- paste(basePath,"data/folds/test/",dataset, fold, ".csv", sep="")
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
                '<entry key="OutputPath">',dataset,fold,'-marc.arules</entry>',
                "</properties>", sep="")
        
        marcFilePath <-paste(basePath,"result/",dataset,fold,".xml",sep="")
        write(x, file = marcFilePath,
              ncolumns = 1,
              append = FALSE, sep = ",")        
      }
      
      #Run and store results from MARC
      rmMARC <- marcExtend(cbaRuleModel=rmCBA,datadf=trainFold,continuousPruning=continuousPruning, postpruning=postpruning, fuzzification=fuzzification, annotate=annotate,loglevel = logger)
      prediction <- predict(rmMARC,testFold,testingType=testingType)
      acc<-CBARuleModelAccuracy(prediction, testFold[[rmMARC@classAtt]])
      message(paste("MARC acc:", acc))
      accSumMARC <-  accSumMARC+ acc
      rules <- rmMARC@ruleCount
      message(paste("MARC rules:",rules))
      ruleSumMARC <- ruleSumMARC + rules
    }
    accCBA <- accSumCBA/10
    accMARC <- accSumMARC/10
    rulesCBA<- ruleSumCBA/10
    rulesMARC <-ruleSumMARC/10
    
    if (resultsFile!="")
    {
          write(c(dataset,accCBA,rulesCBA,accMARC,rulesMARC,rulelearning_options$target_rule_count,rulelearning_options$init_support,rulelearning_options$init_conf,rulelearning_options$conf_step,rulelearning_options$supp_step,rulelearning_options$minlen,rulelearning_options$init_maxlen,rulelearning_options$iteration_timeout,rulelearning_options$total_timeout,rulelearning_options$max_iterations,pruning_options$default_rule_pruning,pruning_options$rule_window,pruning_options$greedy_pruning,continuousPruning,postpruning,fuzzification,annotate,testingType,format(Sys.time(), "%X")), file = resultsFile,
          ncolumns = 24,
          append = TRUE, sep = ",")
    }
    
    if (cbaResultFile!="")
    {
      write(c(dataset,accCBA,rulesCBA), file = oneConfigResultFile,
            ncolumns = 3,
            append = TRUE, sep = ",")     
    }    
    if (oneConfigResultFile!="")
    {
      write(c(dataset,accMARC,rulesMARC), file = oneConfigResultFile,
            ncolumns = 3,
            append = TRUE, sep = ",")     
    }

    
  }
}

doEvalAccDetailed <- function()
{
  resultsFile="result/marc.csv"
  
  if(!file.exists(resultsFile)){
    write(paste("dataset,accCBA,rulesCBA,accMARC,rulesMARC,target_rule_count,init_support,init_conf,conf_step,supp_step,minlen,init_maxlen,iteration_timeout,total_timeout,max_iterations,default_rule_pruning,rule_window,greedy_pruning,continuousPruning,postpruning,fuzzification,annotate,testingType,timestamp"), file = resultsFile,
          ncolumns = 1,
          append = FALSE, sep = ",")
  }
  target_rule_count<-10000
  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalMARC(datasets=datasets,rulelearning_options=list(target_rule_count = target_rule_count, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=TRUE), 
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),
           continuousPruning=TRUE, postpruning=TRUE, fuzzification=TRUE, annotate=TRUE,testingType="mixture",basePath="",resultsFile=resultsFile)
}

doEvalAccMARCOneRule <- function()
{
  
  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalMARC(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=TRUE), 
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),
           continuousPruning=TRUE, postpruning=FALSE, fuzzification=FALSE, annotate=FALSE,testingType="oneRule",basePath="",resultsFile="",oneConfigResultFile="result/MARC-OneRule-accuracy.csv",cbaResultFile= "result/CBA-accuracy.csv")
}


doEvalAccMARCMultiRule <- function()
{
  
  datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")
  evalMARC(datasets=datasets,rulelearning_options=list(target_rule_count = 10000, init_support = 0.00, init_conf = 0.5, conf_step = 0.05, supp_step = 0.05,
                                                       minlen = 2, init_maxlen = 3, iteration_timeout = 2, total_timeout = 100.0, max_iterations = 30, trim=TRUE), 
           pruning_options=list(default_rule_pruning=TRUE, rule_window=100,greedy_pruning=FALSE),
           continuousPruning=FALSE, postpruning=TRUE, fuzzification=FALSE, annotate=TRUE,testingType="mixture",basePath="",resultsFile="",oneConfigResultFile="result/MARC-MultiRule-accuracy.csv",cbaResultFile= "")
}

doEvalTime <- function()
{
  logpath <- "result/MARC-scaling.csv"
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
    evalTimeMARC(trainFold,testFold,foldsize,logpath,iterations=iterations,includeMARC=TRUE)
  }
}

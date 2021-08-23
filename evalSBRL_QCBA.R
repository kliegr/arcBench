#require(devtools)
#on Debian the following may require sudo apt-get install libgsl-dev
#install_version("sbrl", version = "1.2", repos = "http://cran.us.r-project.org")

#this may not run on new versions of R (i.e. R 4 is not supported)
rm(list = ls())
#options(java.parameters = "-Xmx16000m")
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx16192m"))
gc()
library(qCBA)
library(rCBA)
library(sbrl)
library(stringr)
runSeparateCBAQCBA <-FALSE
basePath="./"

datasets <- c("hepatitis","ionosphere","sonar","spambase","australian", "breast-w", "colic", "credit-a",  "diabetes", "heart-statlog","credit-g","kdd1000_","kdd10000_","kdd20000_","kdd30000_","kdd40000_")
classAtt<-"label"
modelsFolder <- "SBRL_QCBA_Models"
dir.create(file.path(basePath, modelsFolder))
SBRLmodelsFolder <- "SBRL_Models"
dir.create(file.path(basePath, SBRLmodelsFolder))
foldsToProcess <- 10
maxFoldIndex  <-foldsToProcess -1
iterations <-1
resultfolder = "./SBRL_results/"
#algs <- c("CBA","QCBA","SBRL","SBRLQCBA")  
algs <- c("SBRL","SBRLQCBA")  
dir.create(file.path(basePath, resultfolder))
#METAPARAM SETTING
SBRL_rule_maxlenRange=c(1,10)
defaultRuleOverlapPruningRange=c("transactionBased","noPruning")
for (SBRL_rule_maxlen in SBRL_rule_maxlenRange){
  print(paste("SBRL_rule_maxlen",SBRL_rule_maxlen))
 for (defaultRuleOverlapPruning in defaultRuleOverlapPruningRange){
  for (dataset in datasets[1:length(datasets)])
  {
    if (dataset == "kdd1000_" | dataset == "kdd1000_" | dataset == "kdd10000_" | dataset == "kdd20000_" | dataset == "kdd30000_" | dataset == "kdd40000_" )
    {
      minCondImprovement<-0
      if (defaultRuleOverlapPruning=="transactionBased")
      {
        print("Skipping transactionBased for kdd datasets ")
        next
      }
    }   
    else{
      minCondImprovement<--1
    }    
    # some datasets would fail with the full max length
    if (SBRL_rule_maxlen==10)
    {
      maxlen_adjusted=10
      if (dataset=="hepatitis")
      {
        maxlen_adjusted<-5
      }
      if (dataset=="credit-g")
      {
        maxlen_adjusted<-5
      }      
      if (dataset=="sonar")
      {
        maxlen_adjusted<-3
      } 
      if (dataset=="ionosphere")
      {
        maxlen_adjusted<-3
      }        
      if (dataset=="spambase")
      {
        maxlen_adjusted<-2
      }   
      if (dataset == "kdd1000_" | dataset == "kdd1000_" | dataset == "kdd10000_" | dataset == "kdd20000_" | dataset == "kdd30000_" | dataset == "kdd40000_" )
      {
        maxlen_adjusted<-3
      }   
      config=paste(defaultRuleOverlapPruning,"-Long",sep="")
    }
    else{
      maxlen_adjusted=1
      config=paste(defaultRuleOverlapPruning,"-1",sep="")

    }
    print(config)
    skip=FALSE 
    algComputed<- c()
    for (alg in algs)
    {

      if (minCondImprovement==-1)
      {
        mciFilenameTAG<-""
      } else
      {
        mciFilenameTAG<-paste0("_mci", minCondImprovement)
      }
      resultfile = paste(resultfolder,alg,"-", config, mciFilenameTAG,".csv",sep="")
      message(paste0("reading from",resultfile))
      if (!file.exists(resultfile))
      {
        write(paste("dataset,accuracy,rules,antlength,buildtime"), file = resultfile,
              ncolumns = 1,
              append = FALSE, sep = ",")
      }   
      file_text <- readLines(resultfile)
      check_result <- TRUE %in% grepl(paste("^",dataset,",",sep=""),file_text)
      if (isTRUE(check_result))
      {
        algComputed <- c(algComputed,TRUE)
      }
      else{
        algComputed <-c(algComputed,FALSE)
      }
     
    }
    if (all(algComputed))
    {
      message(paste("Skipping dataset",dataset, "with config:", config, "(already computed)"))
      next
    }     

    df <- data.frame(matrix(rep(0,12), ncol = 4, nrow = 4), row.names = c("accuracy","rulecount","rulelength","buildtime"))
    colnames(df)<-algs
    for (fold in 0:maxFoldIndex)
    {
      foldTempResultsFile <- paste0("temp_",dataset,config,"_",fold,".csv")
      message(paste("processing:", dataset,fold))
      if (file.exists(foldTempResultsFile))
      {
        df <- utils::read.csv(foldTempResultsFile, header=TRUE, check.names = TRUE, row.names=1)
        message(paste("results read from temp file:", foldTempResultsFile))
        next
      }
      
      trainPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".csv", sep="")
      testPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"test",.Platform$file.se,dataset, fold, ".csv", sep="")
      trainFold <- utils::read.csv(trainPath, header=TRUE, check.names = TRUE)
      testFold <- utils::read.csv(testPath, header=TRUE, check.names = TRUE)
      
      # RENAME TARGET as per sbrl requirements both in TRAIN and TEST fold
      orignames<-colnames(trainFold)
      orignames[length(orignames)]<-classAtt
      colnames(trainFold)<-orignames
      colnames(testFold)<-orignames
     
      # recode label to binary values
      #trainFold$label <- as.numeric(trainFold$label)
      
      # create dict mapping from original distinct class values to 0,1 

      origval<-levels(as.factor(trainFold$label))
      if (startsWith(dataset,"kdd"))
      {
        NORMALCLASS<-1
        NONNORMALCLASS<-0
        message("for kdd dataset converting class label to binary")
        newval<-rep.int(0,length(origval))
        newval[which(origval=="normal.")]<-NORMALCLASS
      }
      else{
        newval<-range(0,1)
      }
     
      dict<-data.frame(origval,newval)
      # apply dict to train and test fold
      trainFold$label<-dict[match(trainFold$label, dict$origval), 2]
      
      testFold$label<-dict[match(testFold$label, dict$origval), 2]
      if (startsWith(dataset,"kdd"))
      {
      # if test data contains unseen value, 
      # a later called dict[match(trainFold$label, dict$origval), 2]  converts
      # it to NA. The following ensures that unseen value is converted to 
      # 0, which is the non-normal class
        testFold$label[is.na(testFold$label)] <- NONNORMALCLASS
      }
      #Discretize training data
      trainFoldDiscTemp <- discrNumeric(trainFold, classAtt)
      trainFoldDiscCutpoints <- trainFoldDiscTemp$cutp
      trainFoldDisc <- as.data.frame(lapply(trainFoldDiscTemp$Disc.data, as.factor))
      
      #Discretize test data
      testFoldDisc <- applyCuts(testFold, trainFoldDiscCutpoints, infinite_bounds=TRUE, labels=TRUE)
      
      # Apply sbrl
      message("SBRL")
      
      start.time <- Sys.time()
      for (i in 1:iterations) sbrl_model <- sbrl(trainFoldDisc, iters=30000, pos_sign="0", 
                         neg_sign="1", rule_minlen=1, rule_maxlen=maxlen_adjusted, 
                         minsupport_pos=0.10, minsupport_neg=0.10, 
                         lambda=10.0, eta=1.0, alpha=c(1,1), nchain=10)
      end.time <- Sys.time()
      df["buildtime","SBRL"] <-df["buildtime","SBRL"]  + round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
      yhat <- predict(sbrl_model, testFoldDisc)
      yvals<- as.integer(yhat$V1>0.5)
      sbrl_acc<-mean(as.integer(yvals == testFoldDisc$label))
      message(paste("acc sbrl",sbrl_acc,"in fold", fold, "rules:",nrow(sbrl_model$rs)))
      df["accuracy","SBRL"]<-df["accuracy","SBRL"]+sbrl_acc
      df["rulecount","SBRL"]<-df["rulecount","SBRL"]+nrow(sbrl_model$rs)

      # gives same result as internal discretization
      if (FALSE)
      {
        # External disc
        message("CBA WITH EXTERNAL DISCRETIZATION")
        rmCBA <- cba(trainFoldDisc, classAtt=classAtt)
        prediction <- predict(rmCBA,testFoldDisc)
        acc_ext <- CBARuleModelAccuracy(prediction, testFoldDisc[[classAtt]])
        message(paste("CBA with external disc acc:",acc_ext))
      }
      
      if (runSeparateCBAQCBA)
      {
        # Built-in disc
        message("CBA WITH BUILT-IN DISCRETIZATION")
        start.time <- Sys.time()
        for (i in 1:iterations) rmCBA <- cba(trainFold, classAtt=classAtt)
        end.time <- Sys.time()
        df["buildtime","CBA"] <-df["buildtime","CBA"]  + round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
        
        prediction <- predict(rmCBA,testFold)
        acc_builtin <- CBARuleModelAccuracy(prediction, testFold[[classAtt]])
        df["accuracy","CBA"]<-df["accuracy","CBA"]+acc_builtin
        df["rulecount","CBA"]<-df["rulecount","CBA"]+length(rmCBA@rules)
        avgtemp <- sum(rmCBA@rules@lhs@data)/length(rmCBA@rules)
        df["rulelength","CBA"]<-df["rulelength","CBA"]+avgtemp
        message(paste("CBA with built in disc acc:",acc_builtin))
        
        # QCBA
        message("*** QCBA")
        start.time <- Sys.time()
        rmQCBA <- qcba(cbaRuleModel=rmCBA,datadf=trainFold,extend="numericOnly",defaultRuleOverlapPruning=defaultRuleOverlapPruning,attributePruning=TRUE,trim_literal_boundaries=TRUE,
                       continuousPruning=FALSE, postpruning="cba",minImprovement=0,
                       minCondImprovement=minCondImprovement)
        end.time <- Sys.time()
        df["buildtime","QCBA"] <-df["buildtime","QCBA"]  + round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
        
        df["rulecount","QCBA"] <- df["rulecount","QCBA"]+ rmQCBA@ruleCount
        prediction <- predict(rmQCBA,testFold)
        acc_qcba <- CBARuleModelAccuracy(prediction, testFold[[rmQCBA@classAtt]])
        df["accuracy","QCBA"]<-df["accuracy","QCBA"]+acc_qcba
        avgtemp <- (sum(unlist(lapply(rmQCBA@rules[1],str_count,pattern=",")))+
                      # assuming the last rule has antecedent length zero - not counting its length
                      nrow(rmQCBA@rules)-1)/nrow(rmQCBA@rules)
        df["rulelength","QCBA"]<-df["rulelength","QCBA"]+avgtemp 
        message(paste("QCBA acc:",acc_qcba, " rules: ", rmQCBA@ruleCount))
        
        
      }

  
      
      message("QCBA ON SBRL OUTPUT")
      #Convert SBRL model to CBA structure
      #rules in the list order with default rule missing
      lhs <- sbrl_model$rulenames[sbrl_model$rs$V1]
      #add default class antecedent
      lhs <- c(lhs,"{}")
      #class probabilities, incl. default rule
      classes<-as.integer(sbrl_model$rs$V2<0.5)
      rulecount<-length(classes)
      rhs<-paste0(rep("{label=",rulecount,),classes,rep("}",rulecount))
      rules<-paste0(lhs, rep(" => ", rulecount), rhs)
      support<- rep(1,rulecount)
      confidence<- rep(1,rulecount)
      dfRules<-data.frame(rules,support,confidence,stringsAsFactors=FALSE)
      
      rm_sbrl<-sbrlModel2arcCBARuleModel(sbrl_model,trainFoldDiscCutpoints,trainFold,"label") 
      write.csv(as(rm_sbrl@rules,"data.frame"), file = paste0(SBRLmodelsFolder, "/",dataset,fold,".csv", sep = "") )
      
      avgtemp <- sum(rm_sbrl@rules@lhs@data)/length(rm_sbrl@rules)
      df["rulelength","SBRL"]<-df["rulelength","SBRL"]+avgtemp
      
      message(paste("*** SBRLQCBA VERSION"))
      
      start.time <- Sys.time()
      for (i in 1:iterations) rmQCBA_sbrl <- qcba(cbaRuleModel=rm_sbrl,datadf=trainFold, extend="numericOnly",defaultRuleOverlapPruning=defaultRuleOverlapPruning,attributePruning=TRUE,trim_literal_boundaries=TRUE,
                                                  continuousPruning=FALSE, postpruning="cba", minImprovement=0,
                                                  minCondImprovement=minCondImprovement,            loglevel = "WARNING")
      end.time <- Sys.time()
      
      averageExecTime<- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
      message(paste("QCBA took:",averageExecTime, "seconds"))
      df["buildtime","SBRLQCBA"] <-df["buildtime","SBRLQCBA"]  + averageExecTime
      prediction <- predict(rmQCBA_sbrl,testFold)
      acc_qcba_sbrl <- CBARuleModelAccuracy(prediction, testFold[[rmQCBA_sbrl@classAtt]])

      message(paste("acc_qcba_sbrl",acc_qcba_sbrl, "in fold", fold, " rules: ", rmQCBA_sbrl@ruleCount))
      df["accuracy","SBRLQCBA"]<-df["accuracy","SBRLQCBA"]+acc_qcba_sbrl
      df["rulecount","SBRLQCBA"] <- df["rulecount","SBRLQCBA"]+ rmQCBA_sbrl@ruleCount
      avgtemp <- (sum(unlist(lapply(rmQCBA_sbrl@rules[1],str_count,pattern=",")))+
                              # assuming the last rule has antecedent length zero - not counting its length
                              nrow(rmQCBA_sbrl@rules)-1)/nrow(rmQCBA_sbrl@rules)
      df["rulelength","SBRLQCBA"]<-df["rulelength","SBRLQCBA"]+avgtemp
      write.csv(rmQCBA_sbrl@rules, file = paste0(modelsFolder, "/",dataset,fold, "-",defaultRuleOverlapPruning,mciFilenameTAG,".csv", sep = "") )
      write.csv(df,file=foldTempResultsFile)
    }

    df<- df * 1/foldsToProcess
    print(df)
    for (alg in algs)
    {
      resultfile = paste(resultfolder,alg,"-", config, mciFilenameTAG,".csv",sep="")
      write(c(dataset,df["accuracy",alg],df["rulecount",alg],df["rulelength",alg],df["buildtime",alg]), file =resultfile,
            ncolumns = 5,
            append = TRUE, sep = ",") 
    }
  }
 }
}

#require(devtools)
#on Debian the following may require sudo apt-get install libgsl-dev
#install_version("sbrl", version = "1.2", repos = "http://cran.us.r-project.org")

#this may not run on new versions of R (i.e. R 4 is not supported)
rm(list = ls())
#options(java.parameters = "-Xmx16000m")
options(java.parameters = c("-Xmx16192m"))
gc()
library(qCBA)
library(rCBA)
library(arulesCBA)
library(sbrl)
library(stringr)


###############################################################################
#This function is present in latest QCBA release and may be removed
arulesCBA2arcCBAModel <- function(arulesCBAModel, cutPoints, rawDataset, classAtt, attTypes )
{
  # note that the example for this function generates a notice
  # this should be fine according to https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Suggested-packages
  
  CBAmodel <- CBARuleModel()
  ruleCount<-length(arulesCBAModel$rules)
  if (sum(arulesCBAModel$rules@lhs@data[,ruleCount])>0)
  {
    #Both LHS and RHS in arules have the same dimension. 
    #positions 1 to number of distinct items in LHS are used for RHS
    #remaining positions are used for RHS items
    if ("default" %in% attributes(arulesCBAModel)$names)
    {
      itemCount<-nrow(arulesCBAModel$rules@lhs@data) #total for LHS and RHS items
      emptyLHS<-rep(FALSE,itemCount)
      arulesCBAModel$rules@lhs@data <- as(cbind(arulesCBAModel$rules@lhs@data,emptyLHS),"ngCMatrix")
      RHSLevels<-nlevels(arulesCBAModel$default)
      rhs <-emptyLHS
      positionOfDefaultRuleInRHSLevels<-as.numeric(arulesCBAModel$default)
      #RHS for default rule has only one bit on which corresponds to the position
      #of the default rule in the item vector
      rhs[itemCount - RHSLevels+positionOfDefaultRuleInRHSLevels] <- TRUE
      arulesCBAModel$rules@rhs@data <- as(cbind(arulesCBAModel$rules@rhs@data,rhs),"ngCMatrix")
      #arules data frame does not contain quality metrics for the default rule
      arulesCBAModel$rules@quality <- rbind(arulesCBAModel$rules@quality, c(0,0,0,0) )
      message("Last rule added based on default specification in the passed model ")
    }
    else
    {
      warning("Last rule is not a default rule with empty antecedent and could 
      not be automatically added as 'default' attribute is missing")
    }
  }
  CBAmodel@rules <- arulesCBAModel$rules
  CBAmodel@cutp <- cutPoints
  CBAmodel@classAtt <- classAtt
  if (missing(attTypes))
  {
    CBAmodel@attTypes <- sapply(rawDataset, class)
  }
  else
  {
    CBAmodel@attTypes = attTypes
  }
  return (CBAmodel)
}
###############################################################################
runSeparateCBAQCBA <-FALSE
basePath="./"
datasets <- c("anneal","australian","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel")

#datasets <- c("hepatitis","ionosphere","sonar","spambase","australian", "breast-w", "colic", "credit-a",  "diabetes", "heart-statlog","credit-g"
              #,"kdd1000_","kdd10000_","kdd20000_","kdd30000_","kdd40000_"
#              )
algs <- c("CMAR","CPAR","PRM","FOIL2")  
foldsToProcess <- 10
maxFoldIndex  <-foldsToProcess -1
iterations <-1

#algs <- c("CBA","QCBA","SBRL","SBRLQCBA")  


#METAPARAM SETTING
# using default settings
defaultRuleOverlapPruningRange=c("noPruning","transactionBased")
for (defaultRuleOverlapPruning in defaultRuleOverlapPruningRange){
  for (dataset in datasets[1:length(datasets)])
  {
    if (dataset == "kdd1000_" | dataset == "kdd1000_" | dataset == "kdd10000_" | dataset == "kdd20000_" | dataset == "kdd30000_" | dataset == "kdd40000_" )
    {
      minCondImprovement<-0
      mciFilenameTAG<-""
      if (defaultRuleOverlapPruning=="transactionBased")
      {
        print("Skipping transactionBased for kdd datasets ")
        next
      }
    }   
    else{
      minCondImprovement<--1
      mciFilenameTAG<-paste0("_mci", minCondImprovement)
    }    
    config <- "default"
    skip=FALSE 
    algComputed<- c()
    for (alg in algs)
    {
      algQCBA<-paste0(alg,"_QCBA")
      modelsFolder <- paste0(alg,"_QCBA_Models")
      dir.create(file.path(basePath, modelsFolder),showWarnings = FALSE)
      ALGmodelsFolder <- paste0(alg,"_Models")
      dir.create(file.path(basePath, ALGmodelsFolder),showWarnings = FALSE)
      resultfolder = paste0("./",alg,"_results") 
      dir.create(file.path(basePath, resultfolder),showWarnings = FALSE)
      
      resultfile_alg = paste(resultfolder,"/",alg,"-", config, mciFilenameTAG,"-",defaultRuleOverlapPruning,".csv",sep="")
      resultfile_qcba = paste(resultfolder,"/",algQCBA,"-", config, mciFilenameTAG,"-",defaultRuleOverlapPruning,".csv",sep="")
      
      message(paste0("reading from ",resultfile_qcba))
      if (!file.exists(resultfile_qcba) | !file.exists(resultfile_alg) )
      {
        write(paste("dataset,accuracy,rules,antlength,buildtime"), file = resultfile_alg,
              ncolumns = 1,
              append = FALSE, sep = ",")
        write(paste("dataset,accuracy,rules,antlength,buildtime"), file = resultfile_qcba,
              ncolumns = 1,
              append = FALSE, sep = ",")
      }   
      file_text <- readLines(resultfile_qcba)
      check_result <- TRUE %in% grepl(paste("^",dataset,",",sep=""),file_text)
      if (isTRUE(check_result))
      {
        algComputed <- c(algComputed,TRUE)
      }
      else{
        algComputed <-c(algComputed,FALSE)
      }
     
    
      if (all(algComputed))
      {
        message(paste("Skipping dataset",dataset, "with config:", config, "(already computed for qcba)"))
        next
      }     
  
      df <- data.frame(matrix(rep(0,8), ncol = 2, nrow = 4), row.names = c("accuracy","rulecount","rulelength","buildtime"))
      colnames(df)<-c(alg,algQCBA)
      for (fold in 0:maxFoldIndex)
      {
        foldTempResultsFile <- paste0("temp_",dataset,"_",config,"_",alg,"_",fold,"_",defaultRuleOverlapPruning,".csv")
        message(paste("processing:", dataset, "FOLD", fold, "by",alg))
        if (file.exists(foldTempResultsFile))
        {
          message(paste("Read temp results for ",alg,"and", algQCBA, "from",foldTempResultsFile))
          dfTemp <- utils::read.csv(foldTempResultsFile, header=TRUE, check.names = TRUE, row.names=1)
          df[,c(alg,algQCBA)] <- dfTemp[,c(alg,algQCBA)]
          df
          #This contains cumulative results up to the fold number, this needs to be divided by the number of folds
          message(paste("skipping the rest of this iteration for curent fold"))
          next
        }
        
        trainPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".csv", sep="")
        testPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"test",.Platform$file.se,dataset, fold, ".csv", sep="")
        trainFold <- utils::read.csv(trainPath, header=TRUE, check.names = TRUE)
        testFold <- utils::read.csv(testPath, header=TRUE, check.names = TRUE)
        classAtt<-colnames(trainFold)[ncol(trainFold)]
        trainFoldDiscTemp <- discrNumeric(trainFold, classAtt)
        trainFoldDiscCutpoints <- trainFoldDiscTemp$cutp
        trainFoldDisc <- as.data.frame(lapply(trainFoldDiscTemp$Disc.data, as.factor))
        
        #Discretize test data
        testFoldDisc <- applyCuts(testFold, trainFoldDiscCutpoints, infinite_bounds=TRUE, labels=TRUE)
         
        start.time <- Sys.time()
        f_rule_model<-get(alg)
        message(paste("Training",alg,"for",iterations,"iterations"))
        for (i in 1:iterations) model <- f_rule_model(as.formula(paste(classAtt, " ~ .")), trainFoldDisc)
        end.time <- Sys.time()
        averageExecTime<-round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
        message(paste(alg,"took:",averageExecTime, "seconds per iteration"))
        df["buildtime",alg] <-df["buildtime",alg]  + averageExecTime
        yhat <- predict(model, testFoldDisc)
        acc<-mean(as.integer(as.character(yhat) == as.character(testFoldDisc[, ncol(testFoldDisc)])))
        message(paste("acc",alg, acc,"in fold", fold, "rules:",length(model$rules)))
        df["accuracy",alg]<-df["accuracy",alg]+acc
        df["rulecount",alg]<-df["rulecount",alg]+length(model$rules)
  
        model_trans<-arulesCBA2arcCBAModel(model,trainFoldDiscCutpoints,trainFold,classAtt) 
        outputfile <- paste0(ALGmodelsFolder, "/",dataset,fold,".csv", sep = "")
        message("writing transformed rules to QCBA format from ", alg, " to ", outputfile)
        write.csv(as(model_trans@rules,"data.frame"), file = outputfile )
        
        avgtemp <- sum(model_trans@rules@lhs@data)/length(model_trans@rules)
        df["rulelength",alg]<-df["rulelength",alg]+avgtemp
        
        message(paste("***", alg,"QCBA VERSION"))
        
        message(paste("Running QCBA ON", alg, "OUTPUT", "for", iterations, "iterations" ))
        start.time <- Sys.time()
        for (i in 1:iterations) rmQCBA <- qcba(cbaRuleModel=model_trans,datadf=trainFold, extend="numericOnly",defaultRuleOverlapPruning=defaultRuleOverlapPruning,attributePruning=TRUE,trim_literal_boundaries=TRUE,
                                                    continuousPruning=FALSE, postpruning="cba", minImprovement=0,
                                                    minCondImprovement=minCondImprovement,loglevel = "WARNING")
        end.time <- Sys.time()
        
        averageExecTime<- round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
        message(paste(algQCBA, "took:",averageExecTime, "seconds per iteration"))
        df["buildtime",algQCBA] <-df["buildtime",algQCBA]  + averageExecTime
        prediction <- predict(rmQCBA,testFold)
        acc_qcba_alg <- CBARuleModelAccuracy(prediction, testFold[[rmQCBA@classAtt]])
  
        message(paste("acc",algQCBA,acc_qcba_alg, "in fold", fold, " rules: ", rmQCBA@ruleCount))
        df["accuracy",algQCBA]<-df["accuracy",algQCBA]+acc_qcba_alg
        df["rulecount",algQCBA] <- df["rulecount",algQCBA]+ rmQCBA@ruleCount
        avgtemp <- (sum(unlist(lapply(rmQCBA@rules[1],str_count,pattern=",")))+
                                # assuming the last rule has antecedent length zero - not counting its length
                                nrow(rmQCBA@rules)-1)/nrow(rmQCBA@rules)
        df["rulelength",algQCBA]<-df["rulelength",algQCBA]+avgtemp
        #This contains cumulative results up to the fold number, this needs to be divided by the number of folds
        outputfile <- paste0(modelsFolder, "/",dataset,fold, "-",defaultRuleOverlapPruning,mciFilenameTAG,".csv", sep = "")
        message(paste('Writing QCBA rules to',outputfile))
        write.csv(rmQCBA@rules, file = outputfile )
        message(paste('Writing intermediate resuts for ', alg, "and",algQCBA, "on dataset", dataset, "and FOLD", fold,  " to ", foldTempResultsFile))
        write.csv(df[,c(alg,algQCBA)],file=foldTempResultsFile)
      }
      message(paste("Finished processing all folds for dataset", dataset, "and algorithm", alg, "(", algQCBA, ")"))
      message("Averaging fold results")
      df<- df * 1/foldsToProcess
      print(df)
      message("Writing average result for all folds of ",dataset, " by ", alg, " to ", resultfile_alg)
      write(c(dataset,df["accuracy",alg],df["rulecount",alg],df["rulelength",alg],df["buildtime",alg]), file =resultfile_alg,
            ncolumns = 5,
            append = TRUE, sep = ",")
      message("Writing average result for all folds of ",dataset, " by ", algQCBA, " to ", resultfile_qcba)
      write(c(dataset,df["accuracy",algQCBA],df["rulecount",algQCBA],df["rulelength",algQCBA],df["buildtime",algQCBA]), file =resultfile_qcba,
            ncolumns = 5,
            append = TRUE, sep = ",") 
      
    }
    message(paste("Finished processing all algorithms for dataset", dataset))
    
  }
  message(paste("Finished processing all algorithms and dataset for default rule pruning setup ", defaultRuleOverlapPruningRange))
}
file.remove(dir(path=".",  pattern="temp_*"))


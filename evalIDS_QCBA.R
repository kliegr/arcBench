library(qCBA)
library(rCBA)
library(stringr)
library(rlist)

#patched frameToRules from rCBA package
frameToRules <- function(model){
  # export quality measures
  quality<-model[,2:4]
  # parse text
  rowItems <- lapply(model$rules,function(x) {
    x <- as.character(x)
    pattern <- "[[:space:]]*\\{(.*)\\}[[:space:]]*=>[[:space:]]*\\{(.*)\\}[[:space:]]*"
    m <- regexec(pattern, x)
    strRule <- regmatches(x, m)
    ant <- strsplit(strRule[[1]][2],",")[[1]]
    cons <- strsplit(strRule[[1]][3],",")[[1]]
    list(ant=ant,cons=cons)
  })
  # unique lhs and rhs items
  antItems <- unique(c(unlist(sapply(rowItems, function(x) x$ant))))
  consItems <- unique(c(unlist(sapply(rowItems, function(x) x$cons))))
  # all items
  items <- c(antItems, consItems)
  # prepare matrices for antecedents(lhs) and consequents(rhs)
  antM <- matrix(0, ncol=length(items), nrow = nrow(model))
  dimnames(antM) <- list(NULL, items)
  consM <- matrix(0, ncol=length(items), nrow = nrow(model))
  dimnames(consM) <- list(NULL, items)
  # set presence of items in lhs and rhs
  sapply(seq_len(nrow(model)), function(x){
    row <- unname(rowItems[x])[[1]]
    antM[x,match(row$ant,items)] <<- 1
    consM[x,match(row$cons,items)] <<- 1
    NULL
  })
  # convert to item matrix
  antI <- as(antM, "itemMatrix")
  consI <- as(consM, "itemMatrix")
  # create rules
  rules <- new("rules", lhs=antI, rhs=consI, quality = quality)
  rules
}
iterations <-1
basePath="."
datasets <- c("australian","anneal","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel","kdd1000_","kdd10000_","kdd20000_","kdd30000_","kdd40000_")
foldsToProcess <- 10
maxFoldIndex  <-foldsToProcess -1
defaultRuleOverlapPruningRange=c("transactionBased","noPruning")
attributePruningRange=c(TRUE,FALSE)
minCondImprovement <- -1 #default is -1

basePath="./"
IDSModelsFolder<-paste(basePath,"IDS_Models",sep="")
resultFolder <- "IDS_results"
modelsFolder <- "QCBA_IDS_Models"
for (attributePruning in attributePruningRange) 
{
  for (defaultRuleOverlapPruning in defaultRuleOverlapPruningRange)
  {
    dir.create(file.path(basePath, resultFolder))
    dir.create(file.path(basePath, modelsFolder))
    print(defaultRuleOverlapPruning)

  
    for (dataset in datasets[1:length(datasets)])
    {
      if (dataset == "kdd1000_" | dataset == "kdd1000_" | dataset == "kdd10000_" | dataset == "kdd20000_" | dataset == "kdd30000_" | dataset == "kdd40000_" )
      {
        minCondImprovement<-0
        if (defaultRuleOverlapPruning=="transactionBased")
        {
          print(dataset)
          print("Skipping transactionBased for kdd datasets ")
          next
        }
      }   
      else{
        minCondImprovement<--1
      }
      if (minCondImprovement==-1)
      {
        mciFilenameTAG<-""
      } else
      {
        mciFilenameTAG<-paste0("_mci", minCondImprovement)
      }      
      mainresultfile <-  paste(resultFolder,"/","IDSQCBA_R_",defaultRuleOverlapPruning,"_ATTPRUNING_",as.character(attributePruning),mciFilenameTAG,".csv",sep="")
      if (!file.exists(mainresultfile))
      {
        write(paste("dataset,accuracy,rules,antlength,buildtime"), file = mainresultfile,
              ncolumns = 1,
              append = FALSE, sep = ",")
      }
      #resultfile= paste("./IDSQCBA_results/",dataset, ".csv",sep="")
      #if (file.exists(resultfile)) next;
  
      file_text <- readLines(mainresultfile)
      check_result <- TRUE %in% grepl(paste("^",dataset,",",sep=""),file_text)
      if (isTRUE(check_result))
      {
        message(paste("Skipping dataset",dataset,"(already computed)"))
        next
      }
  
      df <- data.frame(matrix(rep(0,12), ncol = 1, nrow = 4), row.names = c("accuracy","rulecount","rulelength","buildtime"))
      colnames(df)<-c("IDSQCBA")
  
      for (fold in 0:maxFoldIndex)
      {
        message(paste("loading IDS rules:", dataset,fold))
        idsRulesPath <- paste(IDSModelsFolder,.Platform$file.sep,dataset, fold, ".csv", sep="")
  
        testPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"test",.Platform$file.se,dataset, fold, ".csv", sep="")
        testFold <- utils::read.csv(testPath  , header  =TRUE, check.names = TRUE)
        trainPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".csv", sep="")
        trainFold <- utils::read.csv(trainPath  , header  =TRUE, check.names = TRUE)
        classAtt<- tail(colnames(trainFold),n=1)
  
        trainDiscCutpointsPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_discr2",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".cutpoints", sep="")
        trainFoldDiscCutpoints <- list.unserialize(file=trainDiscCutpointsPath)
  
        dfRulesIDS <- utils::read.csv(idsRulesPath , header  =TRUE, check.names = TRUE)
        rm_ids <- CBARuleModel()
        rm_ids@rules <- frameToRules(dfRulesIDS)
        #rm_sbrl@rules <- as.item.matrix(dfRules,trainFold,classAtt)
        rm_ids@cutp <- trainFoldDiscCutpoints
        rm_ids@classAtt <- classAtt
        rm_ids@attTypes <- sapply(trainFold, class)
        start.time <- Sys.time()
        for (i in 1:iterations) rmQCBA_ids <- qcba(cbaRuleModel=rm_ids,datadf=trainFold, extend="numericOnly",defaultRuleOverlapPruning=defaultRuleOverlapPruning,attributePruning=attributePruning,trim_literal_boundaries=TRUE,
                           continuousPruning=FALSE, postpruning="cba", minImprovement=0,
                           minCondImprovement=-1,            loglevel = "WARNING")
        end.time <- Sys.time()
        df["buildtime","IDSQCBA"] <-df["buildtime","IDSQCBA"]  + round(as.numeric((end.time - start.time)/iterations,units="secs"),2)
        prediction <- predict(rmQCBA_ids,testFold)
        acc_qcba_ids <- CBARuleModelAccuracy(prediction, testFold[[rmQCBA_ids@classAtt]])
        df["accuracy","IDSQCBA"]<-df["accuracy","IDSQCBA"]+acc_qcba_ids
        df["rulecount","IDSQCBA"] <- df["rulecount","IDSQCBA"]+ rmQCBA_ids@ruleCount
        avgtemp <- (sum(unlist(lapply(rmQCBA_ids@rules[1],str_count,pattern=",")))+
                      # assuming the last rule has antecedent length zero - not counting its length
                      nrow(rmQCBA_ids@rules)-1)/nrow(rmQCBA_ids@rules)
        df["rulelength","IDSQCBA"]<-df["rulelength","IDSQCBA"]+avgtemp
        write.csv(rmQCBA_ids@rules, file = paste0(modelsFolder, "/",dataset,fold, "-",defaultRuleOverlapPruning,"_ATTPRUNING_",as.character(attributePruning),".csv", sep = "") )
        message(paste("IDSQCBA acc:",acc_qcba_ids, " rules", rmQCBA_ids@ruleCount))
      }
      df<- df * 1/foldsToProcess
      print(df)
      #write.csv(df, file=resultfile)
  
      write(c(dataset,df["accuracy","IDSQCBA"],df["rulecount","IDSQCBA"],df["rulelength","IDSQCBA"],df["buildtime","IDSQCBA"] ), file = mainresultfile,
            ncolumns = 5,
            append = TRUE, sep = ",")
    }
  
  }
}

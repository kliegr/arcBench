

library(qCBA)
library(rCBA)
library(sbrl)
library(stringr)
library(rlist)
x = 10


basePath="."
datasets <- c("australian","anneal","autos","breast-w","colic","credit-a","credit-g","diabetes","glass","heart-statlog","hepatitis","hypothyroid","ionosphere","iris","labor","letter","lymph","segment","sonar","spambase","vehicle","vowel","kdd1000_","kdd10000_","kdd20000_","kdd30000_","kdd40000_","kdd50000_")
foldsToProcess <- 10
maxFoldIndex  <-foldsToProcess -1
for (dataset in datasets[1:length(datasets)])
{
  for (fold in 0:maxFoldIndex)
  {
    message(paste("discretizing:", dataset,fold))
    trainPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".csv", sep="")
    testPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_nodiscr",.Platform$file.sep,"test",.Platform$file.se,dataset, fold, ".csv", sep="")
    trainDiscPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_discr2",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".csv", sep="")
    trainDiscCutpointsPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_discr2",.Platform$file.sep,"train",.Platform$file.sep,dataset, fold, ".cutpoints", sep="")
    testDiscPath <- paste(basePath,.Platform$file.sep,"data",.Platform$file.sep,"folds_discr2",.Platform$file.sep,"test",.Platform$file.se,dataset, fold, ".csv", sep="")
    if (file.exists(trainDiscPath))
    {
        message(paste("Skipping, already exists",trainDiscPath))
        next
    }
    trainFold <- utils::read.csv(trainPath  , header  =TRUE, check.names = TRUE)
    classAtt<- tail(colnames(trainFold),n=1)
    testFold <- utils::read.csv(testPath  , header  =TRUE, check.names = TRUE)
    #Discretize training data
    trainFoldDiscTemp <- discrNumeric(trainFold, classAtt)
    trainFoldDiscCutpoints <- trainFoldDiscTemp$cutp
    trainFoldDisc <- as.data.frame(lapply(trainFoldDiscTemp$Disc.data, as.factor))
    #Discretize test data
    testFoldDisc <- applyCuts(testFold, trainFoldDiscCutpoints, infinite_bounds=TRUE, labels=TRUE)
    write.csv(trainFoldDisc,trainDiscPath,quote=FALSE,row.names=FALSE)
    write.csv(testFoldDisc,testDiscPath,quote=FALSE,row.names=FALSE)
    list.serialize(trainFoldDiscCutpoints,file=trainDiscCutpointsPath)
    #list.unserialize(file=trainDiscCutpointsPath)
    
  }
}

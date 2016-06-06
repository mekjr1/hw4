#!/bin/env Rscript


library("caret")
library("klaR")
require("e1071")


# read inputs from command line argments


args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw4_104761505_AbdulMeque.R -fold n –out performance.csv
       ", call.=FALSE)
}
# parse parameters
i<-1
while(i < length(args))
{
  if(args[i] == "-fold"){
    fold<-strtoi(args[i+1])
    i<-i+1
  }else if(args[i] == "-out"){
    out<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

# read input data from csv

d<-read.csv("Archaeal_tfpssm.csv", header=FALSE)
if(is.na(d[1,ncol(d)]))
{
  d<-d[,-ncol(d)] #remove the last useless column,
}
#order the data based on column V2, column to be predicted
d<-d[with(d, order(V2)),]
id<-d[,1]


#  n-fold cross validation

index<-1:nrow(d)

testAccuracy<-NULL
calibAccuracy <-NULL
trainAccuracy <-NULL
fold<-10

sampled = sample(1:nrow(d), size=0.9*nrow(d))
d2 <- d[sampled, ]
calib <- d[-sampled, ]
n = floor(nrow(d2)/fold)
for(f in 1:fold){

  beginIndex<-((f-1)*n+1)
  endIndex = (f*n)
  subset=beginIndex:endIndex
  cv.train = d2[-subset,]
  cv.test=d2[subset,]

  x.train <- cv.train[,3:ncol(cv.train)]
  y.train <- as.factor(cv.train[,2])
  x.test <- cv.test[,3:ncol(cv.test)]
  x.calib<-calib[,3:ncol(calib)]
  y.calib<-as.factor(calib[,2])
  y.test <- as.factor(cv.test[,2])
  fit <- svm(x.train,y.train)

  trainPredict<- predict(fit, x.train)
  acct<-confusionMatrix(trainPredict,y.train)
  trainAccuracy[f]<-acct$overall["Accuracy"]

  testPredict <- predict(fit, x.test)
  accT<-confusionMatrix(testPredict,y.test)
  testAccuracy[f]<-accT$overall["Accuracy"]

  calibPredict<- predict(fit, x.calib)
  accC <-confusionMatrix(calibPredict,y.calib)
  calibAccuracy[f]<-accC$overall["Accuracy"]
  #xd= confusionMatrix(prediction,y.test)
  #print(xd$overall["Accuracy"])
}

sets <- c("Training", "Testing", "Calibration")
accuracyRates <-  rbind(which.min(trainAccuracy), which.min(testAccuracy), which.min(calibAccuracy))
results <- cbind(sets,accuracyRates)
colnames(results) <- c("Set","Accuracy")
print (results)

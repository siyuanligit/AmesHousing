load("cleanDF.rdata")
head(df)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(caret)
library(Metrics)
set.seed(0)
dfN = df[,sapply(df,is.numeric) | sapply(df,is.integer)]
dfC = df[,sapply(df,is.character) | sapply(df,is.factor)]
dfC = model.matrix(~ .,data = dfC)[,-1]
dfC = as.data.frame(dfC)
DF = cbind(dfN,dfC)
DF$SalePrice = log(DF$SalePrice)
samps = createDataPartition(DF$SalePrice,p = .8,list = F)
trn = DF[samps,]
tst = DF[-samps,]
mtries = sqrt(nrow(trn))
cont  = trainControl(method = "cv",number = 10)
tune = expand.grid(mtry = mtries)
rforest = train(SalePrice ~ .,data = trn,trControl = cont,tuneGrid = tune)
preds = predict(rforest,newdata = tst,na.action = na.omit)
str(df)
rmsle(exp(tst$SalePrice),exp(preds))

     
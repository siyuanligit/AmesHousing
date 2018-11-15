load("cleanDF.rdata")
head(df)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(caret)

samps = createDataPartition(df$SalePrice,p = .8,list = F)
trn = df[samps,]
tst = df[-samps,]
cont  = trainControl(method = "cv",number = 10)

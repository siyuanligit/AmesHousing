### working directory
setwd("C:/Users/Derek/Google Drive/bootcamp/AmesHousing")

### dependencies
library(tidyverse)
library(h2o)

### load cleaned dataset
load(file = "cleanDF.rdata")

### train/test split
trainID = sample(1:nrow(df), 0.75*nrow(df), replace = FALSE)
train = df[trainID,]
test = df[-trainID,]
predictors = names(df[which(names(df) != "SalePrice")])
response = "SalePrice"

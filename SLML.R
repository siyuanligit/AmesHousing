### working directory
setwd("C:/Users/Derek/Google Drive/bootcamp/AmesHousing")

### dependencies
library(tidyverse)
library(h2o)
library(xgboost)

### load cleaned dataset
load(file = "cleanDF.rdata")

df = df %>% select(-DateSold)

### train/test split
set.seed(0)
trainID = sample(1:nrow(df), 0.75*nrow(df), replace = FALSE)
train = df[trainID,]
test = df[-trainID,]
predictors = names(df[which(names(df) != "SalePrice")])
response = "SalePrice"

### some testing with h2o
h2o.init(ip = "localhost", port = 54321, nthreads = -1)

train2o = as.h2o(train)
test2o = as.h2o(test)

rf = h2o.randomForest(x = predictors, y = response, training_frame = train2o, nfolds = 10, ntrees = 500, seed = 0)
h2o.performance(rf, test2o)

gbm = h2o.gbm(x = predictors, y = response, training_frame = train2o, nfolds = 10, max_depth = 5, ntrees = 500, seed = 0)
h2o.performance(gbm, test2o)

h2o.shutdown()

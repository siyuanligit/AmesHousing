### working directory
setwd("C:/Users/Derek/Google Drive/bootcamp/AmesHousing")

### dependencies
library(tidyverse)
library(psych)
library(h2o)
library(Metrics)
library(caret)
library(gbm)
library(xgboost)
library(mgcv)

### load cleaned dataset
load(file = "cleanDF.rdata")

df = df %>% dplyr::select(-DateSold) %>% filter(GrLivArea < 4000)

# boxcox
qplot(df$SalePrice, geom = "density") +
    geom_histogram(aes(y=..density..), color = "white", fill = "#33CC99", bins = 60) + 
    geom_line(aes(y=..density..), stat = "density") +
    stat_function(fun = dnorm, colour = 'indianred', lwd = 1, 
                  args = list(mean(df$SalePrice), sd(df$SalePrice)))
MASS::boxcox(SalePrice~., data = df) # close to 0, do log

df$SalePrice = log(df$SalePrice)

### train/test split
set.seed(0)
trainID = createDataPartition(df$SalePrice, p=0.8, list=F)

# normalization
df.response = df$SalePrice
df.Yr = df %>% select(contains("Yr"), contains("Year"))
cl = sapply(df, function(x) class(x))
df.numeric = df[,names(df)[which(cl == "integer" | cl == "numeric")]] %>% 
    select(-SalePrice, -contains("Yr"), -contains("Year"))
skew.col = names(df.numeric)[which(abs(describe(df.numeric)$skew) > 2)]
for(i in 1:length(skew.col)){
    df.numeric[, skew.col[i]] = ifelse(min(df.numeric[, skew.col[i]]) == 0,
                                       log(df.numeric[, skew.col[i]] + 1),
                                       log(df.numeric[, skew.col[i]]))
}

dummy = dummyVars(" ~ .", data = df[,names(df)[which(cl == "character" | cl == "factor")]], sep = ".")
df.categorical = data.frame(predict(dummy, newdata = df[,names(df)[which(cl == "character" | cl == "factor")]]))
df.categorical = df.categorical %>% select(-ends_with("No"))

dfSparse = data.frame(cbind(df.categorical, df.numeric, df.Yr, SalePrice = df.response))

trainSparse = dfSparse[trainID,]
testSparse = dfSparse[-trainID,]

predictorsSparse = names(dfSparse[which(names(dfSparse) != "SalePrice")])
response = "SalePrice"

### some testing with h2o
system.time({
    h2o.init(ip = "localhost", port = 54321, nthreads = -1)
})

trainSparse2o = as.h2o(trainSparse)
testSparse2o = as.h2o(testSparse)

# gbm
paramsGBM = list(learn_rate = c(0.01, 0.1),
              max_depth = c(3, 5),
              sample_rate = c(0.75, 1),
              col_sample_rate = c(0.5, 0.75, 1),
              ntrees = c(100, 200, 300, 400, 500))

system.time({
    gbmSparse_grid = h2o.grid("gbm",
                        x = predictorsSparse, y = response, training_frame = trainSparse2o, validation_frame = testSparse2o,
                        seed = 0, grid_id = "gbmSparse_grid", hyper_params = paramsGBM, nfolds = 10)
})

h2o.getGrid(grid_id = "gbmSparse_grid",
            sort_by = "RMSE",
            decreasing = F) # col_sample_rate=0.75, sample_rate=0.75, learn_rate=0.1, max_depth=3, ntree=400 ==> 0.105

system.time({
    gbmFinal = h2o.gbm(x = predictorsSparse, y = response, training_frame = trainSparse2o,
                       ntrees = 400, max_depth = 3, learn_rate = 0.1, sample_rate = 0.75, col_sample_rate = 0.75,
                       seed = 0, nfolds = 10)
}) # 4.48 secs
h2o.performance(gbmFinal, testSparse2o) # RMSE 0.1056

# random forest
paramsRF = list(ntrees = c(100, 200, 300, 400, 500),
                mtries = floor(seq(1, 337, 50)))
system.time({
    rfSparse_grid = h2o.grid("randomForest",
                             x = predictorsSparse, y = response, training_frame = trainSparse2o, validation_frame = testSparse2o,
                             seed = 0, grid_id = "rfSparse_grid", hyper_params = paramsRF, nfolds = 10)
})

h2o.getGrid(grid_id = "rfSparse_grid",
            sort_by = "RMSE",
            decreasing = F) # ntrees=400, mtries=51

system.time({
    rfFinal = h2o.randomForest(x = predictorsSparse, y = response, training_frame = trainSparse2o,
                               ntrees = 400, mtries = 51, seed = 0, nfolds = 10)
}) # 29.05 secs
h2o.performance(rfFinal, testSparse2o) # RMSE 0.1169

# glm
system.time({
    glm = h2o.glm(x = predictorsSparse, y = response, training_frame = trainSparse2o,
                  family = "gaussian", nfolds = 10, alpha = 1, lambda_search = TRUE) # lambda = 0.3788
})

system.time({
    glmFinal = h2o.glm(x = predictorsSparse, y = response, training_frame = trainSparse2o,
                       family = "gaussian", nfolds = 10, alpha = 1, lambda = 0.004078)
}) # 1.5 secs
h2o.performance(glmFinal, testSparse2o) # RMSE 0.0947


# ensenble
system.time({
    ens2o <- h2o.stackedEnsemble(x = predictorsSparse, y = response, training_frame = trainSparse2o,
                                 base_models = list(gbmFinal@model_id,
                                                    rfFinal@model_id,
                                                    glmFinal@model_id))
})
h2o.performance(ens2o, testSparse2o)










# with caret
trainCX = model.matrix(SalePrice ~ ., data = trainSparse)[,-1]
trainCY = trainSparse$SalePrice

testCX = model.matrix(SalePrice ~ ., data = testSparse)[,-1]
testCY = testSparse$SalePrice

system.time({
    gbmNative = caret::train(x = trainCX, y = trainCY,
                         method = "gbm",
                         tuneGrid = expand.grid(
                           n.trees = seq(100, 500, by = 100),
                           interaction.depth = c(1,3,5),
                           shrinkage = c(0.01, 0.1),
                           n.minobsinnode = 10
                         ))}) # 865 secs, ntree=300, depth=5, lr=0.1, terminalNode=10

testResult = predict(gbmNative, newdata = testCX)
rmse(testResult, testCY)

system.time({
    xgboost = caret::train(x = trainCX, y = trainCY,
                             method = "xgbTree",
                             tuneGrid = expand.grid(
                                 nrounds = 100,
                                 max_depth = 5,
                                 eta = 0.1,
                                 gamma = 0,
                                 colsample_bytree = 1,
                                 min_child_weight = 1,
                                 subsample = 1
                             ))}) # 38.46 secs

testResult = predict(xgboost, newdata = testCX)
rmse(testResult, testCY)

system.time({
    gam = caret::train(x = trainCX, y = trainCY,
                       method = "gam")
})

testResult = predict(gam, newdata = testCX)
rmse(testResult, testCY)

# shutdown h2o
h2o.shutdown()

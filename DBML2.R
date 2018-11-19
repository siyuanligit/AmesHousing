library(tidyverse)
library(ggplot2)
library(randomForest)
library(caret)
library(Metrics)
library(doMC)
library(psych)
library(Matrix)
library(glmnet)
library(gbm)
library(caretEnsemble)
library(VIM)
library(xgboost)
registerDoMC(cores = 3)
##train set
load("cleanDF.rdata")
head(df)
##Remove DateSold
df = df %>%
  select(.,-DateSold)

##Remove outliers
df = df[df$GrLivArea <=4000,]

##Log heavily skewed predictors
dscrpt = describe(df[,sapply(df,is.integer)|sapply(df,is.numeric)])
dim(dscrpt)
colnames.2.log = rownames(dscrpt[abs(dscrpt$skew) > 2,])
colnames.2.log
for(i in 1:length(colnames.2.log)) {
  df[, colnames.2.log[i]] = ifelse(min(df[, colnames.2.log[i]]) == 0,
                                   log(df[, colnames.2.log[i]] + 1),
                                   log(df[, colnames.2.log[i]]))
}
df$MSSubClass = as.character(df$MSSubClass)
df$OverallCond = as.character(df$OverallCond)
df$OverallQual = as.character(df$OverallQual)

##Test Set
load("testDF.rdata")
##Remove DateSold
test = test %>%
  select(.,-DateSold)

##Log same predictors predictors
for(i in 1:length(colnames.2.log)) {
  test[, colnames.2.log[i]] = ifelse(min(test[, colnames.2.log[i]]) == 0,
                                      log(test[, colnames.2.log[i]] + 1),
                                      log(test[, colnames.2.log[i]]))
}

test$SalePrice = NA

bigdf = rbind(df,test)
bigdum = dummyVars(~ ., data = bigdf,sep = ".")
bigdf = predict(bigdum,bigdf)
bigdf = as.data.frame(bigdf)
bigdf = bigdf %>%
  select(.,-ends_with("No"))

DF = bigdf %>%
  filter(.,!is.na(SalePrice))

test = bigdf %>%
  filter(.,is.na(SalePrice))

## Split train,test
set.seed(0)
parts = createDataPartition(DF$SalePrice,p = .8,list = F)
trn = DF[parts,]
tst = DF[-parts,]

##Split predictors, predicted
trn_x = model.matrix(SalePrice ~ ., data = trn)[,-1]
trn_y = log(trn$SalePrice)

tst_x = model.matrix(SalePrice ~ ., data = tst)[,-1]
tst_y = log(tst$SalePrice)

#kNN
cont_knn = trainControl(method = "cv", n = 10)
set.seed(0)
train_knn = train(x = trn_x,
                  y = trn_y,
                  method = "knn",
                  trControl = cont_knn,
                  tuneLength = 50)
knn_plot1 = plot(train_knn)
knn_plot1
##shortened grid
tune_knn = expand.grid(k = 10:12)
set.seed(0)
train_knn = train(x = trn_x,
                  y = trn_y,
                  method = "knn",
                  trControl = cont_knn,
                  tuneGrid = tune_knn)
##Best is 12
knn_plot2 = plot(train_knn)
knn_plot2
pred_knn = predict(train_knn,newdata = tst_x)
rmse(pred_knn,tst_y)

##Random Forest
cont_forest  = trainControl(method = "cv",number = 10)
set.seed(0)
rforest = train(
  x = trn_x,
  y = trn_y,
  trControl = cont_forest,
  tuneLength = 10,
  method = "rf",
  preProcess = c("center", "scale"),
  importance = T
)
rf_plot1 = plot(rforest)
varimp_rf1 = varImp(rforest,scale = F)
##Reduced mtry
tune_forest = expand.grid(mtry = seq(66,86,length = 6))
set.seed(0)
rforest = train(
  x = trn_x,
  y = trn_y,
  trControl = cont_forest,
  tuneGrid = tune_forest,
  method = "rf",
  preProcess = c("center", "scale"),
  importance = T
)
rf_plot2 = plot(rforest)

##further reduced
tune_forest = expand.grid(mtry = seq(39,66,length = 10))
set.seed(0)
rforest = train(
  x = trn_x,
  y = trn_y,
  trControl = cont_forest,
  tuneGrid = tune_forest,
  method = "rf",
  preProcess = c("zv","center", "scale"),
  importance = T
)
##Best Mtry is 66
rf_plot3 = plot(rforest)
varimp_rf2 = varImp(rforest,scale = T)
plot(varimp_rf2)
pred_rf = predict(rforest,newdata = tst_x)
rmse(pred_rf,tst_y)
##e-net
cont_enet <- trainControl(method = "cv", number = 10)
set.seed(0)
train_enet <- train(
  x = trn_x,
  y = trn_y,
  method = "glmnet",
  preProc = c("zv","center", "scale"),
  trControl = cont_enet,
  tuneLength = 10
)

pred_enet = predict(train_enet,newdata = tst_x)
rmse(pred_enet,tst_y)

##GBM
cont_GBM = trainControl(method = "cv",number = 10)
tune_GBM = expand.grid(n.trees = seq(100,500,by = 100),
                       interaction.depth = c(1,3,5),
                       shrinkage = c(.01,.5,.1),
                       n.minobsinnode = 10)
set.seed(0)
train_GBM = train(x = trn_x,
                  y = trn_y,
                  trControl = cont_GBM,
                  tuneGrid = tune_GBM,
                  preProc = c("center","scale"),
                  method = "gbm")

pred_GBM = predict(train_GBM,newdata = tst_x)
rmse(pred_GBM,tst_y)

##ENSEMBLE!!!!

cont_ensemble = trainControl(method = "cv",number = 10,savePredictions = "final")

##Set up the tune of all the models to be run
tune_ensemble = list(
  caretModelSpec(
    method = "rf",
    tuneGrid = expand.grid(mtry = 66),
    preProc = c("center", "scale")
  ),
  caretModelSpec(
    method = "glmnet",
    tuneGrid = expand.grid(alpha = 1,lambda = 0.004254007),
    preProc = c("center", "scale")
  ),
  caretModelSpec(
    method = "gbm",
    tuneGrid = expand.grid(n.trees = 200,interaction.depth = 5,shrinkage = .1,n.minobsinnode = 10),
    preProc = c("center", "scale")
  ),
  caretModelSpec(
    method = "knn",
    tuneGrid = expand.grid(k = 12),
    preProc = c("center", "scale")
  )
)

##Attempt to run the model
set.seed(0)
train_ensemble = caretList(
  x = trn_x,
  y = trn_y,
  trControl = cont_ensemble,
  tuneList = tune_ensemble
)
ensemble_results = resamples(train_ensemble)
summary(ensemble_results)
dotplot(ensemble_results)
modelCor(ensemble_results)

stack_control = trainControl(method = "cv", n = 10,savePredictions = "final")
stack_glmnet = caretStack(train_ensemble,method = "glmnet",trControl = stack_control,preProc = c("center","scale"),tuneLength = 10)
pred_stack = predict(stack_glmnet,newdata = tst_x)
rmse(pred_stack,tst_y)
stack_glmnet$model
##Final Prediction
colnames(test)
test$SalePrice = 0
test1 = model.matrix(SalePrice ~ ., data = test)[,-1]
class(test1)
pred_final = predict(stack_glmnet,newdata = test1)
length(pred_final)
Id = seq(1461,2919)
length(Id)
final = data.frame(Id = Id,SalePrice = pred_final)
final$SalePrice = exp(final$SalePrice)
write_csv(final,"finaL_results.csv")

##Run the models on the full train dataset
ftrn_x = model.matrix(SalePrice ~ .,data = DF)[,-1]
ftrn_y = DF$SalePrice
ftrn_y = log(DF$SalePrice)
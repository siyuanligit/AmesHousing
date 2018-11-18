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
load("cleanDF.rdata")
registerDoMC(cores = 3)
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


##Dummycode categorical predictors
dfdum = dummyVars(~ ., data = df,sep = ".")
DF = predict(dfdum,df)
DF = as.data.frame(DF)
colnames(DF)
DF = DF %>%
  select(.,-ends_with("No"))

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

##Random Forest
cont_forest  = trainControl(method = "oob")
tune_forest = expand.grid(mtry = c(sqrt(ncol(trn_x)),ncol(trn_x)/3))
set.seed(0)
system.time({rforest = train(x = trn_x,
                             y = trn_y,
                             trControl = cont_forest,
                             tuneGrid = tune_forest,
                             method = "rf",
                             preProcess = c("center","scale"),
                             importance = T)})
preds_forest = predict(rforest,newdata = tst_x)
rmse(preds_forest,tst_y)

cont_enet <- trainControl(method = "cv", number = 10)
set.seed(0)
train_enet <- train(
  x = trn_x,
  y = trn_y,
  method = "glmnet",
  preProc = c("center", "scale"),
  trControl = cont_enet,
  tuneLength = 10
)

pred_enet = predict(train_enet,newdata = tst_x)
rmse(pred_enet,tst_y)

##GBM
cont_GBM = trainControl(method = "oob")
tune_GBM = expand.grid(n.trees = 500,
                       interaction.depth = seq(1,9, by = 2),
                       shrinkage = .1,
                       n.minobsinnode = 10)
set.seed(0)
train_GBM = train(x = trn_x,
                  y = trn_y,
                  tuneGrid = tune_GBM,
                  preProc = c("center","scale"),
                  method = "gbm")

pred_GBM = predict(train_GBM,newdata = tst_x)
rmse(pred_GBM,tst_y)

cont_knn = trainControl(method = "cv", n = 10)
tune_knn = expand.grid(k = sqrt(nrow(trn_x)))
train_knn = train(x = trn_x,
                  y = trn_y,
                  method = "knn",
                  trControl = cont_knn,
                  tuneGrid = tune_knn)

pred_knn = predict(train_knn,newdata = tst_x)
rmse(pred_knn,tst_y)

##ENSEMBLE!!!!

cont_ensemble = trainControl(method = "cv",number = 10,savePredictions = "final")

##Set up the tune of all the models to be run
tune_ensemble = list(
  caretModelSpec(
    method = "rf",
    tuneGrid = expand.grid(mtry = ncol(trn_x) / 3),
    preProc = c("center", "scale")
  ),
  caretModelSpec(
    method = "glmnet",
    tuneGrid = expand.grid(alpha = 1,lambda = 0.004254007),
    preProc = c("center", "scale")
  ),
  caretModelSpec(
    method = "gbm",
    tuneGrid = expand.grid(n.trees = 300,interaction.depth = 5,shrinkage = .1,n.minobsinnode = 10),
    preProc = c("center", "scale")
  ),
  caretModelSpec(
    method = "knn",
    tuneGrid = expand.grid(k = 34),
    preProc = c("center", "scale")
  )
)

##Attempt to run the model
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
stack_glm = caretStack(train_ensemble,method = "glm",trControl = stack_control,preProc = c("center","scale"))
pred_stack = predict(stack_glm,newdata = tst_x)
rmse(pred_stack,tst_y)

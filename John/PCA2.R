setwd("~/Data_Sci_2k18/AmesHousing")

library(psych)
library(randomForest)
library(caret)
library(car)
library(Metrics)
library(glmnet)
library(dplyr)

doPCA = T
doVIF = T
addDums = T

load(file = "cleanDF.rdata")

df$Age = ifelse(df$Age < 0, 0, df$Age)

#bc boxcox lambda close to zero, see other code
response = select(df, SalePrice)
response$SalePrice = log(response$SalePrice)

df = select(df, -SalePrice)

#Various numerical -> categorical
summary(df)
df$YearBuilt = as.character(df$YearBuilt)
df$YearRemodAdd = as.character(df$YearRemodAdd)
df$GarageYrBlt = as.character(df$GarageYrBlt)
df$GarageCars = as.character(df$GarageCars)
df$MoSold = as.character(df$MoSold)
df$YrSold = as.character(df$YrSold)

df = select(df, -DateSold)

######################################
#normalize
######################################
df_numeric = df[,sapply(df, is.numeric)]
df_cat = df[,!sapply(df, is.numeric)]

log_norm = function(col) {
  if(min(col) <= 0) {
    return(log(col+1))
  } else {
    return(log(col))
  }
}

df_num_log = as.data.frame(sapply(df_numeric, log_norm))

scale_center_num = function(col) {
  data = scale(col, center = T, scale = T)
  return(data)
}

df_norm = as.data.frame(sapply(df_num_log, scale_center_num))

summary(df_norm)

####################################
#Train Test
###################################

set.seed(0)
trainIds = sample(1:nrow(df_norm), floor(nrow(df_norm)*4/5), replace = F)

trainx = as.data.frame(df_norm[trainIds,])
testx = as.data.frame(df_norm[-trainIds,])

trainy = response[trainIds,]
testy = response[-trainIds,]

##################################
#Aliases and VIF on continuous Vars
#################################
if(doVIF) {
  vifmodel = lm(SalePrice ~ ., data = cbind(trainx, trainy))

  thisVIFs = vif(vifmodel)
  
  df_norm = as.data.frame(df_norm[,thisVIFs < 5])
  
  trainx = df_norm[trainIds,]
  testx = df_norm[-trainIds,]
  
  trainy = response[trainIds,]
  testy = response[-trainIds,]
  
}
######################################
#PCA
#####################################

if(doPCA) {
  
  PCAdata = df_norm
  PCAcov = cov(PCAdata)
  
  scree = fa.parallel(PCAcov, #The data in question.
              n.obs = nrow(PCAdata), #Since we supplied a
              #covaraince matrix,need to know n.
              fa = "pc", #Display the eigenvalues for PCA.
              n.iter = 100) #Number of simulated analyses to perform.
  abline(h = 1)
  
  nfacts = scree$ncomp
  
  PCAs = principal(PCAdata, nfactors = nfacts, rotate = "none")
  
  PCAs
  
  PCAscores = as.data.frame(PCAs$scores)
  
  df_norm = PCAscores
  
  trainx = df_norm[trainIds,]
  testx = df_norm[-trainIds,]
  
  trainy = response[trainIds,]
  testy = response[-trainIds,]
  
}

#############################
#Dummy Vars
#############################
if(addDums) {
  allx_dummies = data.frame(predict(dummyVars("~ .", data = df_cat), newdata = df_cat))
  
  
  #split test and train
  trainx_dummies = allx_dummies[trainIds,]
  testx_dummies = allx_dummies[-trainIds,]
  
  #preliminary model
  model_sat = lm(SalePrice ~ ., data = cbind(trainx_dummies, trainy))
  
  #check aliases, remove
  aliases = alias(model_sat, complete = T, partial = T)
  alias_names = rownames(aliases$Complete)
  
  allx_no_alias = select(allx_dummies, -alias_names)
  
  df_norm = cbind(df_norm, allx_no_alias)
  
  trainx = df_norm[trainIds,]
  testx = df_norm[-trainIds,]
  
  trainy = response[trainIds,]
  testy = response[-trainIds,]
  
}
################################
#Multiple Linear Regression, not categories
###############################

lm1 = lm(SalePrice ~ ., data = cbind(trainx, trainy))

summary(lm1)

preds = predict(lm1, newdata = testx)

print(rmse(testy$SalePrice, preds))


#############################
#LASSO
#############################
LASSO_X_model_matr = model.matrix(SalePrice ~ .,
                                  data = cbind(trainx, trainy))[,-1]
LASSO_Y_train = trainy$SalePrice

grid = 10^seq(5, -5, length = 1000)

#  Perform 10-fold cross validation and use set.seed(0) on the training data
set.seed(0)
cv_lasso = cv.glmnet(LASSO_X_model_matr, LASSO_Y_train,
                     alpha = 1, nfolds = 10, lambda = grid)

plot(cv_lasso, main = "Lasso Regression\n")

#extract best lambda
best_lasso_lambda = cv_lasso$lambda.min
#evaluate cvm
min(cv_lasso$cvm)

#make final predictors
test_X_final = testx
test_X_model_matr = model.matrix(~ ., testx)[,-1]

#make predictions
lasso_pred = predict.cv.glmnet(cv_lasso,newx = test_X_model_matr,s = "lambda.min")

print(rmse(lasso_pred,testy$SalePrice))


#################################
#Forest
#################################
set.seed(0)
oob.err = numeric(min(10, ncol(trainx)))
names(trainx) = gsub('3', 'E', names(trainx))
for (mtry in 1:min(10, ncol(trainx))) {
  fit = randomForest(SalePrice ~ ., data = cbind(trainx, trainy),
                     mtry = mtry, importance = TRUE)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}


#Visualizing the OOB error.
plot(1:min(10, ncol(trainx)), oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")


#stuck with 10 trees because took too long to go higher.
#pretty clear that LASSO will win
rf2 = randomForest(SalePrice ~ ., data = cbind(trainx, trainy),
                   mtry = which(oob.err == min(oob.err), arr.ind = TRUE),
                   ntree = 500, importance = TRUE)



#Can visualize a variable importance plot.
importance(rf2)
varImpPlot(rf2)

names(testx) = gsub('3', 'E', names(testx))
rf_preds = predict(rf2, newdata = testx)


print(rmse(rf_preds,testy$SalePrice))

save(df_norm, file = "John/PCAedData.RData")









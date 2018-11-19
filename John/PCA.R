setwd("~/Data_Sci_2k18/AmesHousing")

library(psych)
library(randomForest)
library(caret)
library(car)
library(Metrics)
library(glmnet)
library(kknn)

source("cleaning.r")

scale_center_num = function(data) {
  #Takes a data frame as input and scales and centers only the numeric columns
  data[,unlist(lapply(data, is.numeric))] = 
    scale(data[,unlist(lapply(data, is.numeric))], center = T, scale = T)
  return(data)
}

################################
#Make train and test
########################
set.seed(0)
trainIds = sample(1:nrow(df), floor(nrow(df)*4/5), replace = F)

allx = df[,names(df) != "SalePrice"]

####################################
#munging
####################################
allx = select(allx, -DateSold)
allx$YrSold = as.character(allx$YrSold)
allx$MoSold = as.character(allx$MoSold)

forPCA = scale_center_num(log(allx[,sapply(allx, is.numeric)] + 2))
forDummies = allx[,!(sapply(allx, is.numeric))]

#split test and train
trainx = forPCA[trainIds,]
testx = forPCA[-trainIds,]

trainy = df[trainIds,names(df) == "SalePrice"]
testy = df[-trainIds,names(df) == "SalePrice"]

#preliminary model
model_sat = lm(SalePrice ~ ., data = cbind(trainx, trainy))

#check aliases, remove
aliases = alias(model_sat, complete = T, partial = T)
alias_names = rownames(aliases$Complete)

if(is.null(alias_names)) {
  allx_no_alias = forPCA
} else {
  allx_no_alias = select(forPCA, -alias_names)
}

#Check VIFs, remove where >= 5
model_sat_no_alias = lm(SalePrice ~ ., data = cbind(trainx_no_alias, trainy))
var_infl_facts = vif(model_sat_no_alias)

allx_final = allx_no_alias[, var_infl_facts < 5]



#######################################
#PCA
#####################################
do_PCA = T

PCAdata = allx_final
PCAcov = cov(PCAdata)

fa.parallel(PCAcov, #The data in question.
            n.obs = nrow(PCAdata), #Since we supplied a
                                   #covaraince matrix,need to know n.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1)

nFacts = 10

PCAs = principal(PCAdata, nfactors = nFacts, rotate = "none")

PCAs

PCAscores = as.data.frame(PCAs$scores)

if(do_PCA) {
  allx = cbind(PCAscores, forDummies)
} else {
  allx = cbind(allx_final, forDummies)
}

####################################
#munging part 2
#######################################
allx_dummies = data.frame(predict(dummyVars("~ .", data = allx), newdata = allx))


#split test and train
trainx_dummies = allx_dummies[trainIds,]
testx_dummies = allx_dummies[-trainIds,]

#preliminary model
model_sat = lm(SalePrice ~ ., data = cbind(trainx_dummies, trainy))

#check aliases, remove
aliases = alias(model_sat, complete = T, partial = T)
alias_names = rownames(aliases$Complete)

trainx_no_alias = select(trainx_dummies, -alias_names)
allx_no_alias = select(allx_dummies, -alias_names)

#Check VIFs, remove where >= 5
model_sat_no_alias = lm(SalePrice ~ ., data = cbind(trainx_no_alias, trainy))
var_infl_facts = vif(model_sat_no_alias)

allx_final = allx_no_alias[, var_infl_facts < 5]

df_final = cbind(allx_final, df[,names(df) == "SalePrice"])

df_final$SalePrice = log(df_final$SalePrice)

#split test and train
train = df_final[trainIds,]
test = df_final[-trainIds,]

trainx = select(train, -SalePrice)
trainy = select(train, SalePrice)

testx = select(test, -SalePrice)
testy = select(test, SalePrice)

###########################################
#linear model
###########################################

lmPCA1 = lm(SalePrice ~ ., data = cbind(trainx, trainy))

preds = predict(lmPCA1, newdata = testx)

summary(lmPCA1)

#######################################
#evaluate
#####################################

print(rmse(testy$SalePrice, preds))

#####################################
#LASSO
##################################

#LASSO regression
LASSO_X_model_matr = model.matrix(SalePrice ~ .,
                                  data = cbind(trainx, trainy))[, -1]
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

##################################
#Forest
#################################
set.seed(0)
rf1 = randomForest(SalePrice ~ ., data = cbind(trainx, trainy), importance = TRUE)
rf1

set.seed(0)
oob.err = numeric(nFacts)
for (mtry in 1:floor(sqrt(ncol(trainx)))) {
  fit = randomForest(SalePrice ~ ., data = cbind(trainx, trainy),
                     mtry = mtry, importance = TRUE)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
plot(1:floor(sqrt(ncol(trainx))), oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

rf2 = randomForest(SalePrice ~ ., data = cbind(trainx, trainy),
                   mtry = 35, ntree = 500, importance = TRUE)



#Can visualize a variable importance plot.
importance(rf2)
varImpPlot(rf2)

rf_preds = predict(rf2, newdata = testx)


print(rmse(rf_preds,testy$SalePrice))

################################
#KNN
################################

knn1 = train.kknn(SalePrice ~ ., )





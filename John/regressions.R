#Regression models for Ames Housing NYCDSA 2018, John Bentley

setwd("~/Data_Sci_2k18/AmesHousing")
library(car)
library(caret)
library(glmnet)

#get clean data, don't use if running wrapper
#source("cleaning.r")
#source("test.R")

#make certain test cols into factors, like in df
test$MSSubClass = as.factor(test$MSSubClass)
test$OverallQual = as.factor(test$OverallQual)
test$OverallCond = as.factor(test$OverallCond)

#split x and y

train_Y = select(df, SalePrice)
train_X = select(df, -SalePrice)

scale_center_num = function(data) {
  #Takes a data frame as input and scales and centers only the numeric columns
  data[,unlist(lapply(data, is.numeric))] = 
    scale(data[,unlist(lapply(data, is.numeric))], center = T, scale = T)
  return(data)
}

#scale and center, important for LASSO
train_X_scaled = scale_center_num(train_X)
test_X_scaled = scale_center_num(test)

#order by col names
train_X_scaled = train_X_scaled[, order(names(train_X_scaled))]
test_X_scaled = test_X_scaled[, order(names(test_X_scaled))]

#combine for purposes of making dummy vars
dftotal = rbind(train_X_scaled, test_X_scaled)

#make dummy variables for factors
dftotal_no_date = select(dftotal, -DateSold)
dftotal_dummys = data.frame(predict(dummyVars("~ .", data = dftotal_no_date),
                                    newdata = dftotal_no_date))

#re-split test and train
train_X_dummys = dftotal_dummys[1:nrow(train_X_scaled),]
test_X_dummys = dftotal_dummys[(nrow(train_X_scaled) + 1):nrow(dftotal_dummys),]

#preliminary model
train_data_all = cbind(train_X_dummys, train_Y)
model_sat = lm(SalePrice ~ ., data = train_data_all)

#check aliases, remove
aliases = alias(model_sat, complete = T, partial = T)
alias_names = rownames(aliases$Complete)

train_data_no_alias = select(train_data_all, -alias_names)

#Check VIFs, remove where >= 5
model_sat_no_alias = lm(SalePrice ~ ., data = train_data_no_alias)
var_infl_facts = vif(model_sat_no_alias)

train_data_final = train_data_no_alias[, append(var_infl_facts < 5, T)] #append T to
                                                                  #ensure add SalePrice


# standard linear regression
#BoxCox not applied, should apply box cox if want to use this model.
#LASSO preferred because many insignificant predictors here.
basicReg = lm(SalePrice ~ ., data = train_data_final)
summary(basicReg)

#apply BoxCox on lm before LASSO. (Is this order correct?)
bc = boxCox(basicReg)

lambda_bc = bc$x[which(bc$y == max(bc$y))] #Extracting the best lambda value.
lambda_bc #lambda very close to 0, so just use log(x)


train_data_final_bc = train_data_final %>%
  mutate(., SalePricebc = log(SalePrice)) %>%
  select(., -SalePrice)


#LASSO regression
LASSO_X_model_matr = model.matrix(SalePricebc ~ .,
                                data = train_data_final_bc)[, -1]
LASSO_Y_train = train_data_final_bc$SalePricebc

grid = 10^seq(5, -5, length = 1000)


#  Perform 10-fold cross validation and use set.seed(0) on the training data
set.seed(0)
cv_lasso = cv.glmnet(LASSO_X_model_matr, LASSO_Y_train,
                         alpha = 1, nfolds = 10, lambda = grid)

pred.lasso = predict.cv.glmnet(cv_lasso,newx = LASSO_X_model_matr,s = "lambda.min")

rmsle(exp(pred.lasso),exp(LASSO_Y_train))

plot(cv_lasso, main = "Lasso Regression\n")

#extract best lambda
best_lasso_lambda = cv_lasso$lambda.min
#evaluate cvm
min(cv_lasso$cvm)

#make final predictors
test_X_final = test_X_dummys[,names(test_X_dummys) %in% names(train_data_final)]
test_X_model_matr = model.matrix(~ ., test_X_final)[,-1]


#make predictions
basic_preds = predict(basicReg, newdata = test_X_final)
lasso_pred_bc = predict.cv.glmnet(cv_lasso,newx = test_X_model_matr,s = "lambda.min")

#undo boxcox
preds_final = exp(lasso_pred_bc)

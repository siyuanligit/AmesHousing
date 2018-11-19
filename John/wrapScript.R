#wrapper for JB's LASSO NYCDSA ML project 2018
setwd("~/Data_Sci_2k18/AmesHousing")

library(Metrics)

source("cleaning.r")
#source("test.R")

df_all = df

set.seed(0)
trainIds = sample(1:nrow(df_all), floor(nrow(df_all)*4/5), replace = F)

df = df_all[trainIds,]

actual = df_all[-trainIds,names(df_all) == "SalePrice"]

test = df_all[-trainIds,names(df_all) != "SalePrice"]

source("John/regressions.R")

preds = preds_final


MSE_nolog = mean((actual$SalePrice - preds)^2)

Rsqrd = sum((preds - mean(actual$SalePrice))^2) /
  sum((actual$SalePrice - mean(actual$SalePrice))^2)

Rsqrd = trunc(Rsqrd*100, 2)

source("cleaning.r")
source("test.R")

source("John/regressions.R")


write.csv(preds_final, paste0("John/lasso_preds_", Rsqrd, "PctTestR2.csv"))

print(sqrt(MSE_nolog))

print(rmsle(actual$SalePrice, preds))

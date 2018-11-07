### set working directory
setwd("C:/Users/Derek/Google Drive/bootcamp/Project3")

### load dependencies
library(readr)
library(tidyverse)

### load raw data
train = read_csv("rawData/train.csv")

# check summary statistics
names(train)
summary(train)


# separate numerical and categorical variables
numerical = c("LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2",
              "BsmtUnfSF", "TotalBsmtSF", "1stFlrSF", "2ndFlrSF", "LowQualFinSF",
              "GrLivArea", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", 
              "3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal")
categorical = c("MSSubClass","MSZoning", "Street", "Alley", "LotShape", 
                "LandContour", "Utilities", "LotConfig", "LandSlope", "Neighborhood",
                "Condition1", "Condition2", "BldgType", "HouseStyle", "OverallQual",
                "OverallCond", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd",
                "MasVnrType", "ExterQual", "ExterCond", "Foundation", "BsmtQual",
                "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "Heating",
                "HeatingQC", "CentralAir", "Electrical", "KitchenQual", "Functional",
                "FireplaceQu", "GarageType", "GarageFinish", "GarageQual", "GarageCond",
                "PavedDrive", "PoolQC", "Fence", "MiscFeature", "SaleType", "SaleCondition")
# >>> variables that need to be discussed <<<
debatable = c("YearBuilt", "YearRemodAdd","BsmtFullBath", "BsmtHalfBath", "FullBath", 
              "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces",
              "GarageYrBlt", "GarageCars", "MoSold", "YrSold")
train.numerical = train[,c("Id", numerical, "SalePrice")]
train.categorical = train[,c("Id", categorical, "SalePrice")]
train.debatable = train[,c("Id", debatable, "SalePrice")]

sum(length(numerical), length(categorical), length(debatable))

# histogram of sale price
# hist(train$SalePrice)
# 
# plot(x = train$LotArea, y = train$SalePrice)
# plot(x = train$OverallQual, y = train$SalePrice)
# plot(x = train$OverallCond, y = train$SalePrice)
# plot(x = train$YearBuilt, y = train$SalePrice)
# plot(x = train$`1stFlrSF`, y = train$SalePrice)
# plot(x = train$`2ndFlrSF`, y = train$SalePrice)
# plot(x = train$GrLivArea, y = train$SalePrice)
# plot(x = train$FullBath, y = train$SalePrice)
# plot(x = train$HalfBath, y = train$SalePrice)
# plot(x = train$BedroomAbvGr, y = train$SalePrice)
# plot(x = train$TotRmsAbvGrd, y = train$SalePrice)
# 
# ggplot(data = train, aes(x = as.factor(MSSubClass), y = SalePrice))+
#     geom_boxplot()
# 
# train %>% select(Alley) %>% distinct() %>% pull()

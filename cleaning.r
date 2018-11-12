### set working directory
setwd("C:/Users/Derek/Google Drive/bootcamp/Project3")

### load dependencies
library(readr)
library(tidyverse)
library(psych)

### load raw data
train = read_csv("rawData/train.csv")

# check summary statistics
names(train)
summary(train)

# describe
describe(train[,numerical])

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

# convert factor
newDF = sapply(train.categorical, function(x) x = as.factor(x))
newDF = as.data.frame(newDF)

# convert types
train$Alley[is.na(train$Alley)] = "None"


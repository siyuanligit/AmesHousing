### load dependencies
library(readr)
library(dplyr)
library(tidyverse)
library(psych)
library(h2o)

### load raw data
train = read_csv("../train.csv")
test = read_csv("../test.csv")

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

# separate types for easier cleaning
train.numerical = train[,c("Id", numerical)]
train.categorical = train[,c("Id", categorical)]
train.debatable = train[,c("Id", debatable)]

sum(length(numerical), length(categorical), length(debatable))

# describe
describe(train[,numerical])

# check for columns containing NAs
colnames(train.categorical)[colSums(is.na(train.categorical)) > 0]
colnames(train.numerical)[colSums(is.na(train.numerical)) > 0]
colnames(train.debatable)[colSums(is.na(train.debatable)) > 0]

# look at each column with NAs
train.categorical %>% select(MasVnrType) %>% head(10)
# needs discussion >>> MasVnrType, MasVnrType, LotFrontage, GarageYrBlt <<<

# convert types
train.categorical$Alley[is.na(train.categorical$Alley)] = "None"
train.categorical$BsmtQual[is.na(train.categorical$BsmtQual)] = "No Bsmt"
train.categorical$BsmtCond[is.na(train.categorical$BsmtCond)] = "No Bsmt"
train.categorical$BsmtExposure[is.na(train.categorical$BsmtExposure)] = "No Bsmt"
train.categorical$BsmtFinType1[is.na(train.categorical$BsmtFinType1)] = "No Bsmt"
train.categorical$BsmtFinType2[is.na(train.categorical$BsmtFinType2)] = "No Bsmt"
train.categorical$FireplaceQu[is.na(train.categorical$FireplaceQu)] = "No FrPl"
train.categorical$GarageType[is.na(train.categorical$GarageType)] = "No Grge"
train.categorical$GarageFinish[is.na(train.categorical$GarageFinish)] = "No Grge"
train.categorical$GarageQual[is.na(train.categorical$GarageQual)] = "No Grge"
train.categorical$GarageCond[is.na(train.categorical$GarageCond)] = "No Grge"
train.categorical$PoolQC[is.na(train.categorical$PoolQC)] = "No Pool"
train.categorical$Fence[is.na(train.categorical$Fence)] = "No Fnce"
train.categorical$MiscFeature[is.na(train.categorical$MiscFeature)] = "None"

train$BsmtFullBath[is.na(train$BsmtFullBath)] = "0"
train$BsmtHalfBath[is.na(train$BsmtHalfBath)] = "0"
train$FullBath[is.na(train$FullBath)] = "0"
train$HalfBath[is.na(train$HalfBath)] = "0"
train$TotalBsmtSF[is.na(train$TotalBsmtSF)] = "0"

# convert factor
train.categorical = sapply(train.categorical, function(x) x = as.factor(x))
train.categorical = as.data.frame(train.categorical)
train.categorical$Id = as.character(train.categorical$Id)

# Bathrooms and Total SF
train %>%
  mutate(BsmtHalfBath = as.double(BsmtHalfBath),
         HalfBath = as.double(HalfBath),
         BsmtFullBath = as.double(BsmtFullBath),
         FullBath = as.double(FullBath), 
         BsmtHalfBath = BsmtHalfBath * 0.5,
         HalfBath = HalfBath * 0.5,
         TotBath = FullBath + HalfBath + BsmtFullBath + BsmtHalfBath,
         # SF
         TotalBsmtSF = as.integer(TotalBsmtSF),
         TotSF = GrLivArea + TotalBsmtSF,
         Basement = ifelse(TotalBsmtSF == 0, 0, 1))

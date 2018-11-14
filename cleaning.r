### set working directory
setwd("C:/Users/Derek/Google Drive/bootcamp/Project3")

### load dependencies
library(readr)
library(dplyr)
library(psych)

### load raw data
train = read_csv("rawData/train.csv")
# test = read_csv("rawData/test.csv")

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

# needs discussion >>> MasVnrArea, MasVnrType, LotFrontage<<<
train.numerical %>% select(MasVnrArea) %>% is.na() %>% table()
train.categorical %>% select(MasVnrType) %>% is.na() %>% table()

# convert types
train = train %>% 
    mutate(# numerical 
        
        # categorical
        Alley = ifelse(is.na(Alley), "None", Alley),
        BsmtQual = ifelse(is.na(BsmtQual), "No Bsmt", BsmtQual),
        BsmtCond = ifelse(is.na(BsmtCond), "No Bsmt", BsmtCond),
        BsmtExposure = ifelse(is.na(BsmtExposure), "No Bsmt", BsmtExposure),
        BsmtFinType1 = ifelse(is.na(BsmtFinType1), "No Bsmt", BsmtFinType1),
        BsmtFinType2 = ifelse(is.na(BsmtFinType2), "No Bsmt", BsmtFinType2),
        FireplaceQu = ifelse(is.na(FireplaceQu), "No FrPl", FireplaceQu),
        GarageType = ifelse(is.na(GarageType), "No Grge", GarageType),
        GarageFinish = ifelse(is.na(GarageFinish), "No Grge", GarageFinish),
        GarageQual = ifelse(is.na(GarageQual), "No Grge", GarageQual),
        GarageCond = ifelse(is.na(GarageCond), "No Grge", GarageCond),
        PoolQC = ifelse(is.na(PoolQC), "No Pool", PoolQC),
        Fence = ifelse(is.na(Fence), "No Fnce", Fence),
        MiscFeature = ifelse(is.na(MiscFeature), "None", MiscFeature),
        # misc type
        GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt),
        Age = YrSold - YearRemodAdd
    )

# convert factor
train.categorical = sapply(train.categorical, function(x) x = as.factor(x))
train.categorical = as.data.frame(train.categorical)
train.categorical$Id = as.character(train.categorical$Id)


train %>% select(YearRemodAdd) %>% table()

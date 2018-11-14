### load dependencies
library(readr)
library(dplyr)
library(tidyr)
library(Hmisc)
library(zoo)


### load raw data
df = read_csv("rawData/train.csv")
# test = read_csv("rawData/test.csv")

# check summary statistics
# names(df)
# summary(df)

# separate numerical and categorical variables
# numerical = c("LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2",
#               "BsmtUnfSF", "TotalBsmtSF", "1stFlrSF", "2ndFlrSF", "LowQualFinSF",
#               "GrLivArea", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch",
#               "3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal")
# categorical = c("MSSubClass","MSZoning", "Street", "Alley", "LotShape",
#                 "LandContour", "Utilities", "LotConfig", "LandSlope", "Neighborhood",
#                 "Condition1", "Condition2", "BldgType", "HouseStyle", "OverallQual",
#                 "OverallCond", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd",
#                 "MasVnrType", "ExterQual", "ExterCond", "Foundation", "BsmtQual",
#                 "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "Heating",
#                 "HeatingQC", "CentralAir", "Electrical", "KitchenQual", "Functional",
#                 "FireplaceQu", "GarageType", "GarageFinish", "GarageQual", "GarageCond",
#                 "PavedDrive", "PoolQC", "Fence", "MiscFeature", "SaleType", "SaleCondition")
# debatable = c("YearBuilt", "YearRemodAdd","BsmtFullBath", "BsmtHalfBath", "FullBath",
#               "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces",
#               "GarageYrBlt", "GarageCars", "MoSold", "YrSold")

# separate types for easier cleaning
# df.numerical = df[,c("Id", numerical)]
# df.categorical = df[,c("Id", categorical)]
# df.debatable = df[,c("Id", debatable)]

# needs discussion >>> MasVnrArea, MasVnrType, LotFrontage<<<
# df.numerical %>% select(MasVnrArea) %>% is.na() %>% table()
# df.categorical %>% select(MasVnrType) %>% is.na() %>% table()

# convert types
df = df %>% 
    mutate(
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
        GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt),
        Age = YrSold - YearRemodAdd,
        Remodeled = ifelse(YearBuilt == YearRemodAdd, 0, 1),
        Bsmt = ifelse(BsmtQual == "No Bsmt", 0, 1),
        Garage = ifelse(GarageType == "No Grge", 0, 1),
        TotalBath = BsmtFullBath + 0.5*BsmtHalfBath + FullBath + 0.5*HalfBath,
        Condition.Norm = ifelse(Condition1 == "Norm" | Condition1 == "Norm", 1, 0),
        Condition.Artery = ifelse(Condition1 == "Artery" | Condition1 == "Artery", 1, 0),
        Condition.Feedr = ifelse(Condition1 == "Feedr" | Condition1 == "Feedr", 1, 0),
        Condition.PosA = ifelse(Condition1 == "PosA" | Condition1 == "PosA", 1, 0),
        Condition.PosN = ifelse(Condition1 == "PosN" | Condition1 == "PosN", 1, 0),
        Condition.RRAe = ifelse(Condition1 == "RRAe" | Condition1 == "RRAe", 1, 0),
        Condition.RRAn = ifelse(Condition1 == "RRAn" | Condition1 == "RRAn", 1, 0),
        Condition.RRNe = ifelse(Condition1 == "RRNe" | Condition1 == "RRNe", 1, 0),
        Condition.RRNn = ifelse(Condition1 == "RRNn" | Condition1 == "RRNn", 1, 0),
        TotSF = GrLivArea + TotalBsmtSF,
        Basement = ifelse(TotalBsmtSF == 0, 0, 1),
        DateSold = as.yearmon(paste(df$YrSold, df$MoSold), "%Y %m")) %>% 
    select(-Id, -Utilities, -Condition1, -Condition2)

df.neighborhoodFrontage = df %>% 
    group_by(Neighborhood) %>% 
    summarise(meanLotf = mean(LotFrontage, na.rm = TRUE))
df = df %>% left_join(df.neighborhoodFrontage, by = "Neighborhood") %>% 
    mutate(LotFrontage = ifelse(is.na(LotFrontage), meanLotf, LotFrontage)) %>% 
    select(-meanLotf)

# convert factor
# >>> not working, but probably dont need since ML packages automatically treat character as factor <<<
# df = sapply(categorical, function(x) df[,x] = as.factor(df[,x])) 
# df = as.data.frame(df)
# df.categorical$Id = as.character(df.categorical$Id)

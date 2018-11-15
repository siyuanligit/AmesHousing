### load dependencies
library(readr)
library(dplyr)
library(tidyr)
library(Hmisc)
library(zoo)
library(VIM)
library(corrplot)

### load raw data
df = read_csv("train.csv")
# test = read_csv("rawData/test.csv")

# check summary statistics
names(df)
summary(df)

##Check Missing Data
aggr(df, plot = F)

# convert types
df = df %>% 
  mutate(
    Alley = ifelse(is.na(Alley), "No Alley", Alley),
    MasVnrArea = ifelse(is.na(MasVnrArea), 0, MasVnrArea),
    MasVnrType = ifelse(MasVnrArea == 0, "No Vnr", MasVnrType),
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
    MiscFeature = ifelse(is.na(MiscFeature), "No Feature", MiscFeature),
    GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt),
    Age = YrSold - YearRemodAdd,
    Remodeled = ifelse(YearBuilt == YearRemodAdd, "No", "Yes"),
    Bsmt = ifelse(BsmtQual == "No Bsmt", "No", "Yes"),
    Garage = ifelse(GarageType == "No Grge", "No", "Yes"),
    Pool = ifelse(PoolQC == "No Pool", "No", "Yes"),
    TotalBath = BsmtFullBath + 0.5*BsmtHalfBath + FullBath + 0.5*HalfBath,
    Condition.Norm = ifelse(Condition1 == "Norm" | Condition1 == "Norm", "Yes", "No"),
    Condition.Artery = ifelse(Condition1 == "Artery" | Condition1 == "Artery", "Yes", "No"),
    Condition.Feedr = ifelse(Condition1 == "Feedr" | Condition1 == "Feedr", "Yes", "No"),
    Condition.PosA = ifelse(Condition1 == "PosA" | Condition1 == "PosA", "Yes", "No"),
    Condition.PosN = ifelse(Condition1 == "PosN" | Condition1 == "PosN", "Yes", "No"),
    Condition.RRAe = ifelse(Condition1 == "RRAe" | Condition1 == "RRAe", "Yes", "No"),
    Condition.RRAn = ifelse(Condition1 == "RRAn" | Condition1 == "RRAn", "Yes", "No"),
    Condition.RRNe = ifelse(Condition1 == "RRNe" | Condition1 == "RRNe", "Yes", "No"),
    Condition.RRNn = ifelse(Condition1 == "RRNn" | Condition1 == "RRNn", "Yes", "No"),
    TotSF = GrLivArea + TotalBsmtSF,
    DateSold = as.yearmon(paste(df$YrSold, df$MoSold), "%Y %m"),
    PorchTotSF = WoodDeckSF + OpenPorchSF + EnclosedPorch + `3SsnPorch` + ScreenPorch,
    WoodDeck = ifelse(WoodDeckSF == 0, "No", "Yes"),
    OpenPorch = ifelse(OpenPorchSF == 0, "No", "Yes"),
    EnclosePorch = ifelse(EnclosedPorch == 0, "No", "Yes"),
    ThreePorch = ifelse(`3SsnPorch` == 0, "No", "Yes"),
    SPorch = ifelse(ScreenPorch == 0, "No", "Yes"),
    Electrical = replace_na(Electrical, "SBrkr")) %>% 
  select(-Id, -Condition1, -Condition2)

# imputation of LotFrontage
df.neighborhoodFrontage = df %>% 
  group_by(Neighborhood) %>% 
  summarise(MeanLotf = mean(LotFrontage, na.rm = TRUE))
df = df %>% left_join(df.neighborhoodFrontage, by = "Neighborhood") %>% 
  mutate(LotFrontage = ifelse(is.na(LotFrontage), MeanLotf, LotFrontage)) %>% 
  select(-MeanLotf)

# make MSSubClass character
df$MSSubClass = as.factor(df$MSSubClass)
df$OverallQual = as.factor(df$OverallQual)
df$OverallCond = as.factor(df$OverallCond)

save(df, file = "cleanDF.rdata")

# Check missing
aggr(test, plot = F)

# Correlations with SalePrice

numVars <- which(sapply(df, is.numeric))
numVarsNames <- names(numVars)

allnumVar <- df[, numVars]
corVar <- cor(allnumVar)
cor_sort <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

cor_High <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
corVar <- cor_numVar[cor_High, cor_High]

corrplot.mixed(corVar, tl.col="black", tl.pos = "lt")
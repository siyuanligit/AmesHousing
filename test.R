library(readr)
library(dplyr)
library(tidyr)
library(Hmisc)
library(zoo)
library(VIM)
library(knitr)

test = read_csv("rawData/test.csv")

# check summary statistics
names(test)
summary(test)

##Check Missing Data
aggr(test, plot = F)

# convert types
test = test %>% 
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
    DateSold = as.yearmon(paste(YrSold, MoSold), "%Y %m"),
    WoodDeck = ifelse(WoodDeckSF == 0, "No", "Yes"),
    OpenPorch = ifelse(OpenPorchSF == 0, "No", "Yes"),
    EnclosePorch = ifelse(EnclosedPorch == 0, "No", "Yes"),
    ThreePorch = ifelse(`3SsnPorch` == 0, "No", "Yes"),
    SPorch = ifelse(ScreenPorch == 0, "No", "Yes"),
    Electrical = replace_na(Electrical, "SBrkr"),
    PorchTotSF = WoodDeckSF + OpenPorchSF + EnclosedPorch + `3SsnPorch` + ScreenPorch) %>% 
  select(-Id, -Condition1, -Condition2)

##Check Missing Data
aggr(test, plot = F)

# imputation of LotFrontage
test.neighborhootestrontage = test %>% 
  group_by(Neighborhood) %>% 
  summarise(MeanLotf = mean(LotFrontage, na.rm = TRUE))
test = test %>% left_join(test.neighborhootestrontage, by = "Neighborhood") %>% 
  mutate(LotFrontage = ifelse(is.na(LotFrontage), MeanLotf, LotFrontage)) %>% 
  select(-MeanLotf)

# Utilities - Change NAs to All Utilities
table(test$Utilities)
test[is.na(test$Utilities), c('Utilities', 'Neighborhood')]

test$Utilities[is.na(test$Utilities)] <- "AllPub"

# Exterior Variables
ext = test %>% 
  select(., contains("ext"), Neighborhood) %>% 
  filter(Neighborhood == "Edwards")

test$Exterior1st[is.na(test$Exterior1st)] <- names(sort(-table(ext$Exterior1st)))[1]

test$Exterior2nd[is.na(test$Exterior2nd)] <- names(sort(-table(ext$Exterior2nd)))[1]

# MasVnrType
table(test$MasVnrType)
length(which(is.na(test$MasVnrType)))
test[is.na(test$MasVnrType) & !is.na(test$MasVnrArea), c('MasVnrType', 'MasVnrArea', 'Neighborhood')]

mas = test %>% 
  select(., contains("mas"), Neighborhood) %>% 
  filter(Neighborhood == "Mitchel")

test$MasVnrType[is.na(test$MasVnrType)] <- names(sort(-table(mas$MasVnrType)))[2]

# MSZoning
table(test$MSZoning)
length(which(is.na(test$MSZoning)))
test[is.na(test$MSZoning) & !is.na(test$Neighborhood), c('MSZoning', 'Neighborhood')]

zone = test %>% 
  select(., MSZoning, Neighborhood) %>% 
  filter(Neighborhood == "Mitchel")

table(zone$MSZoning) # RL for Mitchel

test$MSZoning[is.na(test$MSZoning) & test$Neighborhood == "Mitchel"] <- names(sort(-table(zone$MSZoning)))[1]

zone2 = test %>% 
  select(., MSZoning, Neighborhood) %>% 
  filter(Neighborhood == "IDOTRR")

table(zone2$MSZoning) # RM for IDOTRR

test$MSZoning[is.na(test$MSZoning) & test$Neighborhood == "IDOTRR"] <- names(sort(-table(zone2$MSZoning)))[1]

# Basement Variables
test[(is.na(test$BsmtFullBath)|is.na(test$BsmtHalfBath)|is.na(test$BsmtFinSF1)|is.na(test$BsmtFinSF2)|is.na(test$BsmtUnfSF)|is.na(test$TotalBsmtSF)), c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]
test$BsmtFullBath[is.na(test$BsmtFullBath)] <-0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <-0
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <- 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- 0

# Kitchen Variables
table(test$KitchenQual)
length(which(is.na(test$KitchenQual)))
test[is.na(test$KitchenQual) & !is.na(test$KitchenAbvGr), c('KitchenAbvGr', 'KitchenQual', 'Neighborhood')]

kitch = test %>% 
  select(., contains("Kitchen"), Neighborhood) %>% 
  filter(Neighborhood == "ClearCr")

test$KitchenQual[is.na(test$KitchenQual) & test$Neighborhood == "ClearCr"] <- names(sort(-table(kitch$KitchenQual)))[1]

# Functional
table(test$Functional)
length(which(is.na(test$Functional)))
test[is.na(test$Functional) & !is.na(test$Neighborhood), c('Functional', 'Neighborhood')]

funct = test %>% 
  select(., Functional, Neighborhood) %>% 
  filter(Neighborhood == "IDOTRR")

test$Functional[is.na(test$Functional) & test$Neighborhood == "IDOTRR"] <- names(sort(-table(funct$Functional)))[1]

# Garage Variables
test[is.na(test$GarageCars), c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')]

test$GarageCars[is.na(test$GarageCars)] <- 0
test$GarageArea[is.na(test$GarageArea)] <- 0

# SaleType
table(test$SaleType)
length(which(is.na(test$SaleType)))
test[is.na(test$SaleType) & !is.na(test$Neighborhood), c('SaleCondition', 'SaleType', 'Neighborhood')]

sale = test %>% 
  select(., SaleType, SaleCondition, Neighborhood) %>% 
  filter(Neighborhood == "Sawyer", SaleCondition == "Normal")

test$SaleType[is.na(test$SaleType) & test$Neighborhood == "Sawyer" & test$SaleCondition == "Normal"] <- names(sort(-table(sale$SaleType)))[1]

# Total Bath
table(test$TotalBath)
length(which(is.na(test$TotalBath)))
test[is.na(test$TotalBath), c('TotalBath', 'FullBath', 'HalfBath', 'BsmtFullBath', 'BsmtHalfBath')]

test$TotalBath = test$FullBath + test$HalfBath + test$BsmtFullBath + test$BsmtHalfBath

# Total SF
length(which(is.na(test$TotSF)))
test[is.na(test$TotSF), c('TotSF', 'GrLivArea', 'TotalBsmtSF')]

test$TotSF = test$GrLivArea + test$TotalBsmtSF

##Check Missing Data
aggr(test, plot = F)

save(test, file = "cleanTest.rdata")

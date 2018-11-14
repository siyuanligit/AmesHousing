library(tidyverse)
library(ggplot2)
library(psych)
library(car)
library(mice)
library(VIM)
library(RANN)
##Read in the Data
trn = read_csv("rawData/train.csv")
test = read_csv("rawData/test.csv")

##Add SalePrice to test as NA,add final rows to show what set they belonged to

test$SalePrice = NA
trn$Set = "train"
test$Set = "test"
## Rowbind the two datasets

bigdat = rbind(trn,test)

str(bigdat)
##Drop ID

bigdat = bigdat[,-1]
column.names = colnames(bigdat)

##Change MSSubClass to character

bigdat = bigdat %>%
  mutate(.,MSSubClass = as.character(MSSubClass))

##Split the data into types of variables

categorical = bigdat[,sapply(bigdat,is.character)]
numerical = bigdat[,sapply(bigdat,is.integer)]
outcome = numerical$SalePrice

##Remove outcome from numerical
numerical = numerical %>%
  dplyr::select(., -SalePrice)

##Check Missing Data in Categorical
aggr(categorical,plot = F)

##Replace NA's where appropriate
categorical = categorical %>%
  mutate(.,Alley = replace_na(Alley,"No Alley"),
         BsmtQual = replace_na(BsmtQual,"No Bsmt")
         ,BsmtCond = replace_na(BsmtCond,"No Bsmt")
         ,BsmtExposure = replace_na(BsmtExposure,"No Bsmt")
         ,BsmtFinType1 = replace_na(BsmtFinType1,"No Bsmt")
         ,BsmtFinType2 = replace_na(BsmtFinType2,"No Bsmt")
         ,FireplaceQu = replace_na(FireplaceQu,"No Fireplace")
         ,GarageType = replace_na(GarageType,"No Garage")
         ,GarageFinish = replace_na(GarageFinish,"No Garage")
         ,GarageQual = replace_na(GarageQual,"No Garage")
         ,GarageCond = replace_na(GarageCond,"No Garage")
         ,PoolQC = replace_na(PoolQC,"No Pool")
         ,Fence = replace_na(Fence,"No Fence")
         ,MiscFeature = replace_na(MiscFeature,"No Feature"))

##Slap the three back together
bigdat = cbind(numerical,categorical)
bigdat = bigdat[,column.names[-80]]

##Look at Missing Values, then look at specifics
aggr(bigdat,plot = F)
str(bigdat)

##Exterior Vars, Ignore NA's
Exter = bigdat %>%
  dplyr::select(.,contains("Exter"))

Exter[!complete.cases(Exter),]

##Masonry Variables, Assuming any NA in area is Zero
Mas = bigdat %>%
  dplyr::select(.,contains("Mas"))

Mas[!complete.cases(Mas),]

bigdat = bigdat %>%
  mutate(.,MasVnrArea = replace_na(MasVnrArea,0))

##Change Any that have 0 area to "No Veneer" 
Mas = bigdat %>%
  dplyr::select(.,contains("Mas"))

Mas[!complete.cases(Mas),]

bigdat = bigdat %>%
  mutate(.,MasVnrType = case_when(MasVnrArea == 0 ~ "No Veneer",TRUE ~ as.character(MasVnrType)))

##Check for Miss, Impute for final missing variable
Mas = bigdat %>%
  dplyr::select(.,contains("Mas"))

Mas[!complete.cases(Mas),]

##Any Bsmt that has no basemnt should have zero SF
Bsmt = bigdat %>%
  dplyr::select(.,contains("Bsmt"))

Bsmt[!complete.cases(Bsmt),]

bigdat = bigdat %>%
  mutate(.,BsmtFinSF1 = replace_na(BsmtFinSF1,0),
         BsmtFinSF2 = replace_na(BsmtFinSF2,0),
         BsmtUnfSF = replace_na(BsmtUnfSF,0),
         TotalBsmtSF = replace_na(TotalBsmtSF,0),
         BsmtFullBath = replace_na(BsmtFullBath,0),
         BsmtHalfBath = replace_na(BsmtHalfBath,0))

##If no garage, year build should be put into garage year
Garage = bigdat %>%
  dplyr::select(.,contains("Garage"))

Garage[!complete.cases(Garage),]

bigdat = bigdat %>%
  mutate(.,GarageYrBlt = replace_na(YearBuilt))

##Obs. 2577 is odd, needs to be corrected
Garage = bigdat %>%
  dplyr::select(.,contains("Garage"))

Garage[!complete.cases(Garage),]

bigdat[2577,] = bigdat[2577,] %>%
  mutate(.,GarageType = "None",GarageCars = 0, GarageArea = 0)
aggr(bigdat,plot = F)

bigdat$Age = bigdat$YrSold - bigdat$YearRemodAdd
bigdat$TotBath = bigdat$FullBath + bigdat$BsmtFullBath +  (bigdat$HalfBath*.5) + (bigdat$BsmtHalfBath*.5)
bigdat$GarageYN = ifelse(bigdat$YearBuilt == bigdat$GarageYrBlt,"No","Yes")
bigdat$RemodYN = ifelse(bigdat$YearBuilt == bigdat$YearRemodAdd,"No","Yes")
bigdat$BsmtYN = ifelse(bigdat$TotalBsmtSF == 0,"No","Yes")
bigdat$PoolYN = ifelse(bigdat$PoolArea == 0,"No","Yes")
###IGNORE THE BOTTOM, MY OWN MUSINGS!!!
# mice(bigdat)
# sqrt(2919)
# pre.54nn = preProcess(bigdat, method = 'knnImpute', k=54)
# imputed.54nn = kNN(bigdat,k = 54)
# 
# newcategorical = imputed.54nn[,sapply(imputed.54nn,is.factor)]
# newnumerical = imputed.54nn[,sapply(imputed.54nn,is.numeric)]
# descrpt = describe(newnumerical)
# row2log=rownames(descrpt[abs(descrpt$skew)>=1,])
# length(row2log)
# for(i in 1:length(row2log)){
#   newnumerical[,row2log[i]] = log((newnumerical[,row2log[i]] + 1))
# }
# descrpt2 = describe(newnumerical)
# rnames = rownames(descrpt2)
# skews = cbind(rnames,bees = descrpt$skew, boop = descrpt2$skew)

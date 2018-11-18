### load dependencies
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Hmisc)
library(zoo)
library(VIM)
library(corrplot)
library(ggthemes)
library(GGally)
library(caret)
library(xgboost)
library(gdata)
library(outliers)

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
aggr(df, plot = F)

# Remove Outliers

df2 <- df %>% 
  filter(GrLivArea < 4000)

# Correlations with SalePrice

numVars <- which(sapply(df, is.numeric))
numVarsNames <- names(numVars)

allnumVar <- df[, numVars]
corVar <- cor(allnumVar)
cor_sort <- as.matrix(sort(corVar[,'SalePrice'], decreasing = TRUE))

# with abs value > 0.5
cor_High <- names(which(apply(cor_sort, 1, function(x) abs(x)>0.5)))
corVar <- corVar[cor_High, cor_High]

corrplot.mixed(corVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

corrplot(as.matrix(corVar), type = 'upper', method = 'color', 
         addCoef.col = 'black', tl.cex = .7, cl.cex = .7, number.cex = .7)

require(GGally)
lm.plt <- function(data, mapping, ...){
  plt <- ggplot(data = data, mapping = mapping) +
    geom_point(shape = 20, alpha = 0.7, color = 'deepskyblue4') +
    geom_smooth(method=loess, fill="forestgreen", color="forestgreen") +
    geom_smooth(method=lm, fill="red", color="red")
  return(plt)
}

ggpairs(allnumVar, cor_High[1:6], lower = list(continuous = lm.plt))

# Correlation df2

numVars2 <- which(sapply(df2, is.numeric))
numVarsNames2 <- names(numVars2)

allnumVar2 <- df2[, numVars2]
corVar2 <- cor(allnumVar2)
cor_sort2 <- as.matrix(sort(corVar2[,'SalePrice'], decreasing = TRUE))

# with abs value > 0.5
cor_High2 <- names(which(apply(cor_sort2, 1, function(x) abs(x)>0.5)))
corVar2 <- corVar2[cor_High2, cor_High2]

corrplot.mixed(corVar2, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

corrplot(as.matrix(corVar2), type = 'upper', method = 'color', 
         addCoef.col = 'black', tl.cex = .7, cl.cex = .7, number.cex = .7)

require(GGally)
lm.plt <- function(data, mapping, ...){
  plt <- ggplot(data = data, mapping = mapping) +
    geom_point(shape = 20, alpha = 0.7, color = 'deepskyblue4') +
    geom_smooth(method=loess, fill="forestgreen", color="forestgreen") +
    geom_smooth(method=lm, fill="red", color="red")
  return(plt)
}

ggpairs(allnumVar2, cor_High2[1:6], lower = list(continuous = lm.plt))

# SalePrice Count

ggplot(df, aes(x = SalePrice)) +
  geom_histogram(fill = "deepskyblue4", binwidth = 10000) +
  scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = scales::comma)

# Sale Price by Neighborhood Bar plot

ggplot(df, aes(x = reorder(Neighborhood, -SalePrice), y = SalePrice, fill = Neighborhood)) +
  geom_bar(stat = 'summary', fun.y = 'median') +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) + 
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)

ggplot(df2, aes(x = reorder(Neighborhood, SalePrice), y = SalePrice, fill = Neighborhood)) +
  geom_bar(stat = 'summary', fun.y = 'median') +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) + 
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3) +
  labs(x = "Neighborhood")

# Sale Price by Neighborhood Boxplot

ggplot(df, aes(x = reorder(Neighborhood, SalePrice), y = SalePrice, fill = Neighborhood)) +
  geom_boxplot(alpha = .75, size = .25) + 
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1, alpha = .25) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) + 
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)

ggplot(df2, aes(x = reorder(Neighborhood, SalePrice), y = SalePrice, fill = Neighborhood)) +
  geom_boxplot(alpha = .75, size = .25) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) + 
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3) +
  labs(x = "Neighborhood")
  
# Overall Quality Boxplot

ggplot(df, aes(x = OverallQual, y = SalePrice, fill = OverallQual)) +
  geom_boxplot(alpha = .75, size = .25) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1, alpha = .25) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) + 
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)

ggplot(df2, aes(x = OverallQual, y = SalePrice, fill = OverallQual)) +
  geom_boxplot(alpha = .75, size = .25) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) + 
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)

# Total SF Scatterplot

ggplot(df, aes(x = TotSF, y = SalePrice, color = TotSF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

ggplot(df2, aes(x = TotSF, y = SalePrice, color = TotSF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

# Above Ground Living Area Scatterplot

ggplot(df, aes(x = GrLivArea, y = SalePrice, color = GrLivArea)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

ggplot(df2, aes(x = GrLivArea, y = SalePrice, color = GrLivArea)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

# Total Basement SF

ggplot(df, aes(x = TotalBsmtSF, y = SalePrice, color = TotalBsmtSF)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

ggplot(df2, aes(x = TotalBsmtSF, y = SalePrice, color = TotalBsmtSF)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

# Total Bathrooms Scatterplot

ggplot(df, aes(x = TotalBath, y = SalePrice, color = TotalBath)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 6, by = 0.5))

ggplot(df2, aes(x = TotalBath, y = SalePrice, color = TotalBath)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 6, by = 0.5))

# Total Bathrooms Boxplots

ggplot(df, aes(x = TotalBath, y = SalePrice, group = TotalBath, color = TotalBath)) +
  geom_boxplot(alpha = .75, size = .25) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1, alpha = .25) +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 6, by = 0.5))

ggplot(df2, aes(x = TotalBath, y = SalePrice, group = TotalBath, color = TotalBath)) +
  geom_boxplot(alpha = .75, size = .25) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1, alpha = .25) +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 6, by = 0.5))

# Age Scatterplot

ggplot(df, aes(x = -Age, y = SalePrice, color = Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

ggplot(df2, aes(x = -Age, y = SalePrice, color = Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

# Date Sold Scatterplot

ggplot(df, aes(x = DateSold, y = SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

ggplot(df2, aes(x = DateSold, y = SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

# Year Built Boxplot

ggplot(df, aes(x = as.factor(YearBuilt), y = SalePrice, color = YearBuilt)) + 
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_discrete(breaks = seq(1870, 2010, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df2, aes(x = as.factor(YearBuilt), y = SalePrice, color = YearBuilt)) + 
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_discrete(breaks = seq(1870, 2010, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Year Built Scatterplot

ggplot(df, aes(x = as.factor(YearBuilt), y = SalePrice, color = YearBuilt)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_discrete(breaks = seq(1870, 2010, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df2, aes(x = as.factor(YearBuilt), y = SalePrice, color = YearBuilt)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_discrete(breaks = seq(1870, 2010, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year Built")

# Year Remodeled Boxplot

ggplot(df, aes(x = as.factor(YearRemodAdd), y = SalePrice, color = YearRemodAdd)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_discrete(breaks = seq(1950, 2010, 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df2, aes(x = as.factor(YearRemodAdd), y = SalePrice, color = YearRemodAdd)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_discrete(breaks = seq(1950, 2010, 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Year Remodeled Scatterplot

ggplot(df, aes(x = as.factor(YearRemodAdd), y = SalePrice, color = YearRemodAdd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_discrete(breaks = seq(1950, 2010, 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df2, aes(x = as.factor(YearRemodAdd), y = SalePrice, color = YearRemodAdd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  scale_x_discrete(breaks = seq(1950, 2010, 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Average and Median Sale Price
series <- df %>% 
  group_by(., DateSold) %>% 
  summarise(., mean = mean(SalePrice), median = median(SalePrice), n = n())

ggplot(series, aes(x = DateSold)) +
  geom_line(aes(y = mean), color = 'lightcoral', size = 1) +
  geom_line(aes(y = median), color = 'deepskyblue4', size = 1) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

series2 <- df2 %>% 
  group_by(., DateSold) %>% 
  summarise(., mean = mean(SalePrice), median = median(SalePrice), n = n())

ggplot(series2, aes(x = DateSold)) +
  geom_line(aes(y = mean, color = 'Mean')) +
  geom_line(aes(y = median, color = 'Median')) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  labs(y = "Sale Price") + scale_colour_manual("Housing Prices", values = c("lightcoral", "deepskyblue4"))

# MSSubClass

df3 <- df

df3$MSSubClass <- recode(df3$MSSubClass, '20'='1 story 1946+', '30'='1 story 1945-',
                         '40'='1 story unf attic', '45'='1,5 story unf',
                         '50'='1,5 story fin', '60'='2 story 1946+',
                         '70'='2 story 1945-', '75'='2,5 story all ages', 
                         '80'='split/multi level', '85'='split foyer', 
                         '90'='duplex all style/age', '120'='1 story PUD 1946+', 
                         '150'='1,5 story PUD all', '160'='2 story PUD 1946+', 
                         '180'='PUD multilevel', '190'='2 family conversion')

ggplot(df3, aes(x = MSSubClass, y = SalePrice, fill = MSSubClass)) +
  geom_bar(stat = 'summary', fun.y = 'median') +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)

ggplot(df3, aes(x = MSSubClass, y = SalePrice, fill = MSSubClass)) +
  geom_boxplot(alpha = .75, size = .25) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1, alpha = .25) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)

df4 <- df2

df4$MSSubClass <- recode(df4$MSSubClass, '20'='1 story 1946+', '30'='1 story 1945-',
                         '40'='1 story unf attic', '45'='1,5 story unf',
                         '50'='1,5 story fin', '60'='2 story 1946+',
                         '70'='2 story 1945-', '75'='2,5 story all ages', 
                         '80'='split/multi level', '85'='split foyer', 
                         '90'='duplex all style/age', '120'='1 story PUD 1946+', 
                         '150'='1,5 story PUD all', '160'='2 story PUD 1946+', 
                         '180'='PUD multilevel', '190'='2 family conversion')

ggplot(df4, aes(x = reorder(MSSubClass, -SalePrice), y = SalePrice, fill = MSSubClass)) +
  geom_bar(stat = 'summary', fun.y = 'median') +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)

ggplot(df4, aes(x = reorder(MSSubClass, SalePrice), y = SalePrice, fill = MSSubClass)) +
  geom_boxplot(alpha = .75, size = .25) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3) +
  labs(x = "MSSubClass")
  
# Garage

ggplot(df, aes(x = GarageCars, y = SalePrice, group = GarageCars, fill = GarageCars)) +
  geom_boxplot(alpha = .75, size = .25) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1, alpha = .25) +
  theme(legend.position="none") +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)

ggplot(df, aes(x = GarageArea, y = SalePrice, color = GarageArea)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)

ggplot(df2, aes(x = GarageCars, y = SalePrice, group = GarageCars, fill = GarageCars)) +
  geom_boxplot(alpha = .75, size = .25) +
  theme(legend.position="none") +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3)

ggplot(df2, aes(x = GarageArea, y = SalePrice, color = GarageArea)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = scales::comma)


# PCA

df_tot <- df[1:nrow(df),]
df_tot$SalePrice <- log(df_tot$SalePrice)

df_tot <- as.data.frame(sapply(df_tot, as.numeric))
testdf_tot <- df[(nrow(df)+1):nrow(df),]

df.ctrl <- trainControl(method="repeatedcv",number=10,
                        repeats=10,
                        verboseIter=FALSE)

test.data <- predict(pca,newdata=test.data.transformed)
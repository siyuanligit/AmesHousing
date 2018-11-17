### working directory
setwd("C:/Users/Derek/Google Drive/bootcamp/AmesHousing")

### dependencies
library(tidyverse)
library(psych)
library(h2o)
library(xgboost)
library(caret)
library(glmnet)

### load cleaned dataset
load(file = "cleanDF.rdata")

df = df %>% select(-DateSold)

# boxcox
qplot(df$SalePrice, geom = "density") +
    geom_histogram(aes(y=..density..), color = "white", fill = "#33CC99", bins = 60) + 
    geom_line(aes(y=..density..), stat = "density") +
    stat_function(fun = dnorm, colour = 'indianred', lwd = 1, 
                  args = list(mean(df$SalePrice), sd(df$SalePrice)))
MASS::boxcox(SalePrice~., data = df) # close to 0, do log

df$SalePrice = log(df$SalePrice)

# gridz = expand.grid(alpha = 1, lambda = 1)
# cont = trainControl(method = "cv",number = 10)
# caret::train(SalePrice ~ .,
#              data = train,
#              method = "glmnet",
#              tuneGrid = gridz,
#              trControl = cont)

### train/test split
set.seed(0)
trainID = sample(1:nrow(df), 0.75*nrow(df), replace = FALSE)
train = df[trainID,]
test = df[-trainID,]
predictors = names(df[which(names(df) != "SalePrice")])
response = "SalePrice"

### some testing of gbm with h2o
system.time({
    h2o.init(ip = "localhost", port = 54321, nthreads = -1)
})

train2o = as.h2o(train)
test2o = as.h2o(test)
system.time({
    gbm = h2o.gbm(x = predictors, y = response, training_frame = train2o, ntrees = 100, max_depth = 5, seed = 0)
})

h2o.performance(gbm, train2o)
h2o.performance(gbm, test2o)

# grid search
params = list(learn_rate = c(0.01, 0.1),
              max_depth = c(3, 5, 7),
              sample_rate = c(0.75, 1),
              col_sample_rate = c(0.5, 0.75, 1),
              ntrees = c(100, 250, 500))
system.time({
    gbm_grid = h2o.grid("gbm",
                        x = predictors, y = response, training_frame = train2o, validation_frame = test2o,
                        seed = 0, grid_id = "gbm_grid", hyper_params = params)
})

h2o.getGrid(grid_id = "gbm_grid",
            sort_by = "RMSE",
            decreasing = F)

system.time({
    glm = h2o.glm(x = predictors, y = response, training_frame = train2o,
                  nfold = 10, alpha = 1, lambda = 0.1)
})
h2o.performance(glm, train2o)

system.time({
    gbmF = h2o.gbm(x = predictors, y = response, training_frame = train2o,
                   col_sample_rate = 0.5, ntrees = 100, max_depth = 5, seed = 0)
})
h2o.performance(gbmF, train2o)
h2o.performance(gbmF, test2o)

for (col in names(df)){
    if (class(df[,col]) == "Character"){
        df[,col] = as.factor(df[,col])
    }
}


library(gbm)
gbmNative = gbm(SalePrice ~., data = df, n.trees = 100, interaction.depth = 5)








# normalization
df.response = df$SalePrice
df.Yr = df %>% select(contains("Yr"), contains("Year"))
cl = sapply(df, function(x) class(x))
df.numeric = df[,names(df)[which(cl == "integer" | cl == "numeric")]] %>% 
    select(-SalePrice, -contains("Yr"), -contains("Year"))
skew.col = names(df.numeric)[which(describe(df.numeric)$skew > 2 | describe(df.numeric)$skew < -2)]
for(col in names(skew.col)){
    if(0 %in% df.numeric[, col]) {
        df.numeric[,col] <- log(1 + df.numeric[,col])
    }
    else {
        df.numeric[,col] <- log(df.numeric[,col])
    }
}

dummy = dummyVars(" ~ .", data = df[,names(df)[which(cl == "character" | cl == "factor")]], sep = ".")
df.categorical = data.frame(predict(dummy, newdata = df[,names(df)[which(cl == "character" | cl == "factor")]]))
df.categorical = df.categorical %>% select(-ends_with("No"))

dfSparse = data.frame(cbind(df.categorical, df.numeric, df.Yr, SalePrice = df.response))
sapply(dfSparse, function(x) class(x))

# box-cox
dfSparse$SalePrice = log(dfSparse$SalePrice)

# xgboost
trainSparse = dfSparse[trainID,]
testSparse = dfSparse[-trainID,]














# shutdown h2o
h2o.shutdown()

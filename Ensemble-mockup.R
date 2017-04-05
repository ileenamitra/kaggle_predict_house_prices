library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
dfPath <- "D:/GIT/kaggle_predict_house_prices/cleaned_data/clean.csv"
df <- read.csv(dfPath)

library(caret)
library(pROC)
library(Metrics)

set.seed(888)
df <- df[sample(nrow(df)),]
df <- df[c(-1)]
df$SalePrice <- log(df$SalePrice)

split <- floor(nrow(df)/3)
ensembleData <- df[0:split,]
blenderData <- df[(split+1):(split*2),]
testingData <- df[(split*2+1):nrow(df),]

Y <- 'SalePrice'
X <- names(ensembleData)[names(ensembleData) != Y]

myControl <- trainControl(method='cv', number=3, repeats=1, returnResamp='none')

model_gbm <- train(ensembleData[,X], ensembleData[,Y], method='gbm', trControl=myControl)
model_pcr <- train(ensembleData[,X], ensembleData[,Y], method='pcr', trControl=myControl)
model_rf <- train(ensembleData[,X], ensembleData[,Y], method='rf', trControl=myControl)

blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,X])
blenderData$rf_PROB <- predict(object=model_rf, blenderData[,X])
blenderData$pcr_PROB <- predict(object=model_pcr, blenderData[,X])

testingData$gbm_PROB <- predict(object=model_gbm, testingData[,X])
testingData$rf_PROB <- predict(object=model_rf, testingData[,X])
testingData$pcr_PROB <- predict(object=model_pcr, testingData[,X])

X <- names(blenderData)[names(blenderData) != Y]
ensemble_model <- train(blenderData[,X], blenderData[,Y], method='gbm', trControl=myControl)

preds <- predict(object=ensemble_model, testingData[,X])

rmse(testingData$SalePrice,preds)

stopCluster(cl)

auc_ensemble <- multiclass.roc(testingData[,Y], preds)
ensemble_perf <- getTrainPerf(ensemble_model)

notDummyData <- read.csv('clean.csv', header=T)

expanded_data <-read.csv('/Users/Emma/Documents/kaggle_predict_house_prices/cleaned_data/cleandmy.csv', header=T)

#get all numeric (continuous) variables
num <- data[sapply(data, is.numeric)]


#save salePrice as vector
salePrice <-notDummyData$SalePrice

#do I need to do salePrice + 1 here?
logSalePrice <-log(salePrice)

#edit data to remove first column(repeated id's) and last column(sale price)
expanded_data$X <-NULL
expanded_data$X.1 <-NULL
expanded_data$SalePrice <- NULL

#should I remove data that has few observations? Like less than 10 observations?

#model all variables, summarize, and decide what to throw out


#tells me there are 81 cases of incomplete data?
which(!complete.cases(expanded_data))


#model with dummy data (cleandmy.csv)
firstModel <- lm(logSalePrice~., data=expanded_data)
summary(firstModel)


#model with not-dummy data(clean.csv)
secondModel <-lm(logSalePrice~., data=notDummyData)
summary(secondModel)

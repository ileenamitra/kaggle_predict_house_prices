train<-read.csv('C:/Users/Keven/Documents/MATH 189/Final/train.csv',header=TRUE)
head(train)
clean=train
train_na=colSums(sapply(train,is.na))
train_na=train_na[train_na>0]
train_na
#Generic functions for imputation
data_impute=function(column='column',value='value')
  train[is.na(train[column]),column]=value
#Imputation for lot frontage
LotFrontage_wna=which(!is.na(train['LotFrontage']))
cor(train$LotFrontage[LotFrontage_wna],train$LotArea[LotFrontage_wna])
cor(train$LotFrontage[LotFrontage_wna],sqrt(train$LotArea[LotFrontage_wna]))
#The observed correlation for LotFrontage vs the sqrt of LotArea is high, so we are using the sqrt of LotArea values to impute
LotFrontage_na=which(is.na(train['LotFrontage']))
sqrtLotArea=sqrt(train$LotArea[LotFrontage_na])
clean$LotFrontage[LotFrontage_na]=sqrtLotArea
min(clean$LotFrontage)
max(clean$LotFrontage)
#Imputation for Alley
clean$Alley<-as.character(clean$Alley)
clean$Alley[is.na(clean$Alley)]<-'NoAccess'
#Imputation for MasVnrType
cmp=which(is.na(train['MasVnrType']))==which(is.na(train['MasVnrArea']))
which(cmp==FALSE)
#The NAs are same for both the columns, so we tke that the mason veneer isn't present in the house
clean$MasVnrType<-as.character(clean$MasVnrType)
clean$MasVnrType[is.na(clean$MasVnrType)]<-'None'
clean$MasVnrArea[is.na(clean$MasVnrArea)]<-0
#Imputation for the basement paramteres
#NA here implies that there is no basement, so we can simply assign 'None'
clean$BsmtQual<-as.character(clean$BsmtQual)
clean$BsmtQual[is.na(clean$BsmtQual)]<-'None'
clean$BsmtCond<-as.character(clean$BsmtCond)
clean$BsmtCond[is.na(clean$BsmtCond)]<-'None'
clean$BsmtExposure<-as.character(clean$BsmtExposure)
clean$BsmtExposure[is.na(clean$BsmtExposure)]<-'None'
clean$BsmtFinType1<-as.character(clean$BsmtFinType1)
clean$BsmtFinType1[is.na(clean$BsmtFinType1)]<-'None'
clean$BsmtFinType2<-as.character(clean$BsmtFinType2)
clean$BsmtFinType2[is.na(clean$BsmtFinType2)]<-'None'
#Imputation for Electrical
summary(train$Electrical)
#Since only one value is missing, we can assign the most occured value to it
clean$Electrical<-as.character(clean$Electrical)
clean$Electrical[is.na(clean$Electrical)]<-'SBrkr'

#writing the cleaned data into a csv file "train1.csv"
write.csv(clean, file="train1.csv")

dfpath <- 'C:/Users/Keven/Documents/MATH 189/Final/train1.csv'
df <- read.csv(dfpath)

## Part 2: Imputation

#Fireplace 
summary(df$FireplaceQu)
df$FireplaceQu <- as.character(df$FireplaceQu)
df$FireplaceQu[is.na(df$FireplaceQu)] <- 'None'
df$FireplaceQu <- as.factor(df$FireplaceQu)

#Garage
summary(df$GarageType)
df$GarageType <- as.character(df$GarageType)
df$GarageType[is.na(df$GarageType)] <- 'None'
df$GarageType <- as.factor(df$GarageType)

df$GarageYrBlt[is.na(df$GarageYrBlt)] <- 0 

summary(df$GarageFinish)
df$GarageFinish <- as.character(df$GarageFinish)
df$GarageFinish[is.na(df$GarageFinish)] <- 'None'
df$GarageFinish <- as.factor(df$GarageFinish)

summary(df$GarageQual)
df$GarageQual <- as.character(df$GarageQual)
df$GarageQual[is.na(df$GarageQual)] <- 'None'
df$GarageQual <- as.factor(df$GarageQual)

summary(df$GarageCond)
df$GarageCond <- as.character(df$GarageCond)
df$GarageCond[is.na(df$GarageCond)] <- 'None'
df$GarageCond <- as.factor(df$GarageCond)

#Pool
summary(df$PoolQC)
df$PoolQC <- as.character(df$PoolQC)
df$PoolQC[is.na(df$PoolQC)] <- 'None'
df$PoolQC <- as.factor(df$PoolQC)

#Fence
summary(df$Fence)
df$Fence <- as.character(df$Fence)
df$Fence[is.na(df$Fence)] <- 'None'
df$Fence <- as.factor(df$Fence)

#Misc
summary(df$MiscFeature)
df$MiscFeature <- as.character(df$MiscFeature)
df$MiscFeature[is.na(df$MiscFeature)] <- 'None'
df$MiscFeature <- as.factor(df$MiscFeature)

#writing the cleaned data into a csv file "clean.csv"
write.csv(df, file="clean.csv")


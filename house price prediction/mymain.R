rm(list=ls())
#library
library(xgboost)
library(caret)
library(forcats)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#read the data
train = read.csv('train.csv')
test = read.csv('test.csv')

#first combine train and test
test$Sale_Price = NA
data = rbind(train, test)
#combine small amount levels
#neighborhood
levels(data$Neighborhood)[levels(data$Neighborhood) %in% c('Landmark', 'Green_Hills', 'Greens', 'Blueste', 'Northpark_Villa')] = 'others'
#MS_Subclass
levels(data$MS_SubClass)[levels(data$MS_SubClass) %in% c('One_and_Half_Story_PUD_All_Ages', 'One_Story_with_Finished_Attic_All_Ages', 'PUD_Multilevel_Split_Level_Foyer', 'One_and_Half_Story_Unfinished_All_Ages')] = 'others'
#Exterior_1st
levels(data$Exterior_1st)[levels(data$Exterior_1st) %in% c('ImStucc', 'PreCast', 'AsphShn', 'CBlock', 'Stone', 'BrkComm')] = 'others'
#Exterior_2nd
levels(data$Exterior_2nd)[levels(data$Exterior_2nd) %in% c('Other', 'PreCast', 'CBlock', 'AsphShn', 'Stone')] = 'others'
#Mas_Vnr_Type
levels(data$Mas_Vnr_Type)[levels(data$Mas_Vnr_Type) %in% c('CBlock', 'BrkCmn')] = 'others'
#Kitchen_Qual
levels(data$Mas_Vnr_Type)[levels(data$Mas_Vnr_Type) %in% c('Poor', 'Fair')] = 'other'
#Misc_Feature
levels(data$Misc_Feature)[levels(data$Misc_Feature) %in% c('Elev', 'TenC', 'Othr', 'Gar2')] = 'others'
#Electrical
levels(data$Electrical)[levels(data$Electrical) %in% c('Mix', 'Unknown', 'FuseP')] = 'others'
#Sale_Type
levels(data$Sale_Type)[levels(data$Sale_Type) %in% c('VWD', 'Con', 'Oth', 'ConLw', 'ConLI', 'CWD') ] = 'others'
#MS_Zoning
levels(data$MS_Zoning)[levels(data$MS_Zoning) %in% c('A_agr', 'I_all', 'C_all') ] = 'others'
#Functional
levels(data$Functional)[levels(data$Functional) %in% c('Sal', 'Sev', 'Maj2') ] = 'others'
#Exter_Cond
levels(data$Exter_Cond)[levels(data$Exter_Cond) %in% c('Poor', 'Excellent') ] = 'other'

train = data[!is.na(data$Sale_Price), ]
test = data[is.na(data$Sale_Price), ]
test$Sale_Price = NULL

#preprocess data
test_label = test$PID
test$PID = NULL
train$PID = NULL

#Drop dominate categorical level and longitude and latitude
dominate_levels = c('Street', 'Utilities', 'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC','Longitude', 'Latitude')
train = train[,!(names(train) %in% dominate_levels)]
test = test[,!(names(test) %in% dominate_levels)]

#Log transform Sale_Price NEED CHANGE WHEN FINAL TEST HAVE NO SALE_PRICE
train_price = log(train$Sale_Price)

#train data
#seperate numeric and factor
is.fact = sapply(within(train, rm('Sale_Price')), is.factor)
factors.df = within(train, rm('Sale_Price'))[, is.fact]
numeric.df = within(train, rm('Sale_Price'))[, !is.fact]

#test data
#seperate numeric and factor    NEED CHANGE WHEN FINAL TEST HAVE NO SALE_PRICE
test_factors.df = test[, is.fact]
test_numeric.df = test[, !is.fact]

#Transform numeric, 'center' and scale
PreNum = preProcess(numeric.df, method=c("center", "scale"))
train_num = predict(PreNum, numeric.df)

#Transform factors to dummy variables
dmy = dummyVars('~.', factors.df)
train_fac = data.frame(predict(dmy, newdata = factors.df))


#Transform numeric, 'center' and scale
test_num = predict(PreNum, test_numeric.df)

#Transform factors to dummy variables
test_fac = data.frame(predict(dmy, newdata = test_factors.df))

#Colum combine
train = cbind(train_fac, train_num)
test = cbind(test_fac, test_num)
dtrain <- xgb.DMatrix(data = as.matrix(train), label= train_price)
dtest <- xgb.DMatrix(data = as.matrix(test))

##First xgboost
time1 = Sys.time()
set.seed(9301)

default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.01, #default = 0.3
  gamma=0,
  max_depth=5, #default=6
  min_child_weight=5, #default=1
  subsample=0.5,
  colsample_bytree=1
)


# xgbcv <- xgb.cv(params = default_param, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F, verbose = 1)
xgb_mod = xgb.train(data = dtrain, params = default_param, nrounds = 2000)
# xgb_mod <- xgb.train(data = dtrain,eta=0.01,watchlist=list(train=dtrain, test=dtest), min_child_weight=5, subsample=0.65, nrounds = 2000, verbose = 1, print_every_n = 500)
prediction = predict(xgb_mod, newdata = dtest)
prediction = exp(prediction)
df = data.frame(test_label, prediction)
colnames(df) = c('PID', 'Sale_Price')
write.table(df, file = 'mysubmission1.txt', quote = FALSE, row.names = FALSE, sep = ',')
time2 = Sys.time()
print(time2 - time1)
##Second xgboost
set.seed(666)
time3 = Sys.time()
default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.01, #default = 0.3
  gamma=0,
  min_child_weight=5, #default=1
  subsample=0.8,
  colsample_bytree=1
)

# xgbcv <- xgb.cv(params = default_param, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F, verbose = 1)
xgb_mod = xgb.train(data = dtrain, params = default_param, nrounds = 2000)
# xgb_mod <- xgb.train(data = dtrain,eta=0.01,watchlist=list(train=dtrain, test=dtest), min_child_weight=5, subsample=0.65, nrounds = 2000, verbose = 1, print_every_n = 500)
prediction = predict(xgb_mod, newdata = dtest)
prediction = exp(prediction)
df = data.frame(test_label, prediction)
colnames(df) = c('PID', 'Sale_Price')
write.table(df, file = 'mysubmission2.txt', quote = FALSE, row.names = FALSE, sep = ',')
time4 = Sys.time()
print(time4 -time3)
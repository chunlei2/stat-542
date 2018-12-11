library(plyr)
library(dplyr)
library(xgboost)
library(caret)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
train = read.csv('train.csv')
test = read.csv('test.csv')
my_test_id = test$id
test$loan_status = NA
mydata = rbind(train, test)

# Data preprocess
## missing value
col_mean = colMeans(is.na(mydata))
col_na = col_mean[which(col_mean > 0)]
na_col_names = names(col_na) # seven variables having missing values
### emp_title, too many categories, drop it

### emp_length, convert to inegers, mean imputation for missing values
mydata$emp_length = as.character(mydata$emp_length)
mydata$emp_length = replace(mydata$emp_length, mydata$emp_length == '10+ years', '10 years')
mydata$emp_length = replace(mydata$emp_length, mydata$emp_length == '< 1 year', '0 year')
emp_length_to_int = function(s){
  if (is.na(s)) {
    s
  }
  else{
    s = as.numeric(strsplit(s, ' ')[[1]][1])
    s
  }
}
mydata$emp_length = sapply(mydata$emp_length, emp_length_to_int)
mydata$emp_length = as.numeric(mydata$emp_length)
emp_length_mean = mean(mydata$emp_length, na.rm = TRUE)
impute_length_na_mean = function(s){
  if (is.na(s)) {
    s = emp_length_mean
    s
  }
  else{
    s
  }
}
mydata$emp_length = sapply(mydata$emp_length, impute_length_na_mean)

### title, too many categories

### dti, outliers may exist, inpute missing value with mean
dti_mean = mean(mydata$dti, na.rm = TRUE)
impute_dti_na_mean = function(s){
  if (is.na(s)) {
    s = dti_mean
    s
  }
  else{
    s
  }
}
mydata$dti = sapply(mydata$dti, impute_dti_na_mean)

### revol_util, impute missing value with mean
revol_util_mean = mean(mydata$revol_util, na.rm = TRUE)
impute_revol_util_na_mean = function(s){
  if (is.na(s)) {
    s = revol_util_mean
    s
  }
  else{
    s
  }
}
mydata$revol_util = sapply(mydata$revol_util, impute_revol_util_na_mean)

### mort_acc, impute missing value with mode
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
mort_acc_mode = Mode(mydata$mort_acc, na.rm = T)
impute_mort_acc_na_mode = function(s){
  if (is.na(s)) {
    s = mort_acc_mode
    s
  }
  else{
    s
  }
}
mydata$mort_acc = sapply(mydata$mort_acc, impute_mort_acc_na_mode)

### pub_rec_bankruptcies
pub_rec_bankruptcies_mode = Mode(mydata$pub_rec_bankruptcies, na.rm = T)
impute_pub_rec_bankruptcies_na_mode = function(s){
  if (is.na(s)) {
    s = pub_rec_bankruptcies_mode
    s
  }
  else{
    s
  }
}
mydata$pub_rec_bankruptcies = sapply(mydata$pub_rec_bankruptcies, impute_pub_rec_bankruptcies_na_mode)

drop_list = c('emp_title', 'title')

mydata = mydata[, !names(mydata) %in% drop_list]


# Preprocess all variables
## term, to integer
mydata$term = sapply(mydata$term, function(x) as.integer(strsplit(as.character(x), ' ')[[1]][1]))

## the grade variable is implied by subgrade, drop grade

## the home_ownership, replace any and none with 'other'
mydata$home_ownership = as.character(mydata$home_ownership)
mydata$home_ownership = replace(mydata$home_ownership,which(mydata$home_ownership == 'ANY'), 'OTHER')
mydata$home_ownership = replace(mydata$home_ownership,which(mydata$home_ownership == 'NONE'), 'OTHER')
mydata$home_ownership = as.factor(mydata$home_ownership)

## annual_int, the variable is highly skewed, so log transform
mydata$annual_inc = log(mydata$annual_inc + 1)

## loan_status, replace default with charged off
mydata$loan_status = as.character(mydata$loan_status)
mydata$loan_status = replace(mydata$loan_status, which(mydata$loan_status == 'Default'), 'Charged Off')
mydata$loan_status = replace(mydata$loan_status, which(mydata$loan_status == 'Charged Off'), 1)
mydata$loan_status = replace(mydata$loan_status, which(mydata$loan_status == 'Fully Paid'), 0)
mydata$loan_status = as.integer(mydata$loan_status)

## zip_code, zip_code implies same thing as addr_state

## earliest_cr_line, extract year for simplicity
mydata$earliest_cr_line = as.character(mydata$earliest_cr_line)
mydata$earliest_cr_line = sapply(mydata$earliest_cr_line, function(x) as.integer(substr(x, nchar(x)-3, nchar(x))))

## fico_range_low and fico_range_high is highly correlated, use the average of them
mydata$fico_score = 0.5*mydata$fico_range_high + 0.5*mydata$fico_range_low

## revol_bal
mydata$revol_bal = log(mydata$revol_bal+1)

## id will be kept for seperating train and test
id = mydata$id
loan_status = mydata$loan_status
drop_list = c('grade', 'zip_code', 'fico_range_high', 'fico_range_low', 'id', 'loan_status')

mydata = mydata[, !names(mydata) %in% drop_list]

## factor variables
is.fact = sapply(mydata, is.factor)
fact.df = mydata[, is.fact]
num.df = mydata[, !is.fact]

## process
preNum = preProcess(num.df, method = c('center', 'scale'))
num.df = predict(preNum, num.df)

## dummy
dmy = dummyVars('~.', fact.df)
fact.df = predict(dmy, fact.df)
mydata = cbind(fact.df, num.df)
mydata = cbind(id, mydata)
mydata = cbind(mydata, loan_status)



raw_test = mydata[mydata$id %in% my_test_id, ]
my_test_id = raw_test$id

raw_train = mydata[!mydata$id %in% my_test_id, ]
test = within(raw_test, rm('id'))
train = within(raw_train, rm('id'))
dtrain = xgb.DMatrix(data = as.matrix(within(train, rm('loan_status'))), label = train$loan_status)
dtest = xgb.DMatrix(data = as.matrix(within(test, rm('loan_status'))))

param = list(
  objective = 'binary:logistic',
  eval_metric = 'logloss',
  max_depth = 7,
  eta = 0.2475,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  min_child_weight = 4
)
set.seed(419)
bst <- xgb.train(data=dtrain,nrounds=131, params = param)
pred = predict(bst, dtest)
pred = cbind(my_test_id, pred)
colnames(pred) = c('id', 'prob')
write.table(pred, file = 'mysubmission1.txt', row.names = F , quote = F, sep = ',')


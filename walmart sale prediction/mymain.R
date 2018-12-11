#rm(list = ls())
library(lattice)
library(dplyr)
# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "lubridate",
  "forecast",
  "tidyverse"
)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
train=read.csv("train.csv")
test=read.csv("test.csv")
# train <- readr::read_csv('train.csv')
# test <- readr::read_csv('test.csv', col_types = list(
#   Weekly_Pred1 = col_double(),
#   Weekly_Pred2 = col_double(),
#   Weekly_Pred3 = col_double()
# ))
# ## How many depts?
# length(unique(train$Dept))
# ## How many stores?
# length(unique(train$Store))
# 
# dept_store_train = table(train$Dept,train$Store)
# dept_store_train
# dept_store_test = table(test$Dept, test$Store)
# dim(dept_store_train)
# dim(dept_store_test)
# 
# ## 125 departments in the test do not have historical data
# tmp=(dept_store_train ==0 )*(dept_store_test>0)
# sum(tmp)  
# 
# ## Names for the 81 departments
# dept.names = sort(unique(train$Dept))
# 
# missing_dept_store = which(tmp>0, arr.ind=TRUE, useNames = FALSE)
# missing_dept_store[,1] = dept.names[missing_dept_store[,1]]
# missing_dept_store = missing_dept_store[order(missing_dept_store[,1], missing_dept_store[,2]),]
# 
# # order the missing dept+store by stores and dept
# missing_dept_store

# change date type
train$Date = as.Date(train$Date, '%Y-%m-%d')
test$Date = as.Date(test$Date, '%Y-%m-%d')

# train$Yr = year(train$Date)
# test$Yr = year(test$Date)
# 
# train$Mon = month(train$Date)
# test$Mon = month(test$Date)
# 
# table(train$Yr, train$Mon)
# table(test$Yr, test$Mon)
# 
# # key adjustment for the week
# train.wk = train$Date
# train.wk = train.wk - train.wk[1]  # date is now 0, 7, 14, ...
# train.wk = train.wk/7 + 5  # make 2010-2-5 as '5', and date becomes continuous integers, i.e., 5, 6, 7, ...
# train.wk = as.numeric(train.wk) %% 52  ## 52 weeks in a year
# train$Wk = train.wk
# 
# test.wk = test$Date
# test.wk = test.wk - test.wk[1]
# test.wk = test.wk/7 + 9 # make 2011-03-04 as '9'.
# test.wk = as.numeric(test.wk) %% 52
# test$Wk = test.wk

# Build the model
## Build necessary function
# converts a Date x num_store forecast to a dataframe
# with Date, Store, value = Weekly_Price columns
flatten_forecast <- function(f_model) {
  f_model %>%
    gather(Store, value, -Date, convert = TRUE)
}


# Adds forecasts to the testing dataframe
update_forecast <- function(test_month, dept_preds, dept, num_model) {
  dept_preds <- flatten_forecast(dept_preds) # date, store, value
  
  pred.d <- test_month %>%
    filter(Dept == dept) %>%
    select('Store', 'Date') %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  pred.d.idx <- test_month$Dept == dept
  pred.d <- test_month[pred.d.idx, c('Store', 'Date')] %>%
    left_join(dept_preds, by = c('Store', 'Date')) # one folder, one department
  
  if (num_model == 1) {
    test_month$Weekly_Pred1[pred.d.idx] <- pred.d$value
  } else if(num_model == 2) {
    test_month$Weekly_Pred2[pred.d.idx] <- pred.d$value
  } else {
    test_month$Weekly_Pred3[pred.d.idx] <- pred.d$value
  }
  
  test_month
}

# update forecasts in the global test dataframe
update_test <- function(test_month) {
  test <<- test %>%
    dplyr::left_join(test_month,
                     by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>% # after left join, there will be .x, .y represent two sources
    mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>% # coalesce ~ or
    mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
    mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
    select(-Weekly_Pred1.x, -Weekly_Pred1.y,
           -Weekly_Pred2.x, -Weekly_Pred2.y,
           -Weekly_Pred3.x, -Weekly_Pred3.y)
}

shift <- function(test, threshold=1.1, shift=1){
  # This function executes a shift of the sales forecasts in the Christmas
  # period to reflect that the models are weekly, and that the day of the week
  # that Christmas occurs on shifts later into the week containing the holiday.
  #
  # NB: Train is actually not used here. Previously, there were other post-
  #     adjustments which did use it, and it is taken in here to preserve a 
  #     calling signature.
  #
  # args:
  # train - this is an n_weeks x n_stores matrix of values of Weekly_Sales
  #         for the training set within department, across all the stores
  # test - this is a (forecast horizon) x n_stores matrix of Weekly_Sales
  #        for the training set within department, across all the stores
  # threshold - the shift is executed if the mean of Weekly_Sales for weeks
  #          49-51 is greater than that for weeks 48 and 52 by at least
  #          a ratio of threshold
  # shift - The number of days to shift sales around Christmas.
  #         Should be 2 if the model is based on the last year only,
  #         or 2.5 if it uses both years
  #
  # returns:
  #  the test data 
  # s <- ts(rep(0,39), frequency=52, start=c(2012,44))
  # idx <- cycle(s) %in% 48:52
  idx <- 5:9
  holiday <- test[idx, 2:46] # five weeks * 45stores 
  baseline <- mean(rowMeans(holiday[c(1, 5), ], na.rm=TRUE)) #mean of the 48 and 52 weeks sale
  surge <- mean(rowMeans(holiday[2:4, ], na.rm=TRUE)) #mean of 49, 50, 51 sale
  holiday[is.na(holiday)] <- 0
  if(is.finite(surge/baseline) & surge/baseline > threshold){
    shifted.sales <- ((7-shift)/7) * holiday #6/7 of the original, 5*45
    shifted.sales[2:5, ] <- shifted.sales[2:5, ] + (shift/7) * holiday[1:4, ]
    shifted.sales[1, ] <- holiday[1, ]
    test[idx, 2:46] <- shifted.sales
  }
  test
}
##### Model Building Functions #####

# Forecasts out the last observation in the training data
naive_model<- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- naive(store_ts, num_forecasts)$mean
  }
  test_ts
}

# Linear model
linear_model <- function(train_ts, test_ts) {
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # linear forecast per store
  for (j in 2:ncol(train_ts)) {
    store_ts <- ts(train_ts[, j], frequency = 52)
    model <- tslm(store_ts ~ trend + season)
    fc <- forecast(model, h = num_forecasts)
    test_ts[, j] = as.numeric(fc$mean)
  }
  test_ts
}

##### Prediction Loop #####

mypredict <- function() {
  ###### Create train and test time-series #######
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_test)
  }
  
  # filter test data.frame for the month that needs predictions
  # backtesting starts during March 2011
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_month <- test %>%
    filter(Date >= start_date & Date < end_date)
  
  # Dates are not the same across months!
  test_dates <- unique(test_month$Date)
  num_test_dates <- length(test_dates)
  
  # Not all stores may need predictions either
  all_stores <- unique(test_month$Store)
  num_stores <- length(all_stores)
  
  # Most importantly not all departments need predictions
  test_depts <- unique(test_month$Dept)
  
  # Dateframe with (num_test_dates x num_stores) rows
  test_frame <- data.frame(
    Date=rep(test_dates, num_stores),
    Store=rep(all_stores, each=num_test_dates)
  )
  
  # Create the same dataframe for the training data
  # (num_train_dates x num_stores)
  train_dates <- unique(train$Date)
  num_train_dates <- length(train_dates)
  train_frame <- data.frame(
    Date=rep(train_dates, num_stores),
    Store=rep(all_stores, each=num_train_dates)
  )
  
  # Model one, Naive model
  #### Perform a individual forecasts for each department
  for (dept in test_depts) {
    # filter for the particular department in the training data
    train_dept_ts <- train %>%
      filter(Dept == dept) %>%
      select(Store, Date, Weekly_Sales)

    # Reformat so that each column is a weekly time-series for that
    # store's department.
    # The dataframe has a shape (num_train_dates, num_stores)
    train_dept_ts <- train_frame %>%
      left_join(train_dept_ts, by = c('Date', 'Store')) %>%
      spread(Store, Weekly_Sales)

    # We create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    test_dept_ts <- test_frame %>%
      mutate(Weekly_Sales = 0) %>%
      spread(Store, Weekly_Sales)

    ###### Model Fitting / Forecasting ######

    # naive forecast
    f_naive <- naive_model(train_dept_ts, test_dept_ts)
    test_month <- update_forecast(test_month, f_naive, dept, 1)

  }
  # update global test dataframe
  update_test(test_month)
  
  # Model two
  #### Perform a individual forecasts for each department
  for (dept in test_depts) {
    # filter for the particular department in the training data
    train_dept_ts <- train %>%
      filter(Dept == dept) %>%
      select(Store, Date, Weekly_Sales)

    # Reformat so that each column is a weekly time-series for that
    # store's department.
    # The dataframe has a shape (num_train_dates, num_stores)
    train_dept_ts <- train_frame %>%
      left_join(train_dept_ts, by = c('Date', 'Store')) %>%
      spread(Store, Weekly_Sales)

    # We create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    test_dept_ts <- test_frame %>%
      mutate(Weekly_Sales = 0) %>%
      spread(Store, Weekly_Sales)

    ###### Model Fitting / Forecasting ######

    # naive forecast
    f_linear <- linear_model(train_dept_ts, test_dept_ts)
    test_month <- update_forecast(test_month, f_linear, dept, 2)

  }

  # update global test dataframe
  update_test(test_month)
  
  # Model three
  #This model is similar to model two, except there is a post_process for folder 5, as folder 5 contains 
  # Chiristmas day, how many days in the week before Christmas matters a lot, our train is from 2010-02, to 
  # 2011-02, our test is from 2011-03, 2012-10. For train data, Chrismasday is on Saturday, so there is 
  # no pre-shopping day in that week, while for test data, Christmasday is on Sunday, so there is one pre-shopping
  # day in that week. When we predict the test data using train data, we might need to shift part of 
  # sales into Christmas week.
  #### Perform a individual forecasts for each department
  for (dept in test_depts) {
    if (t == 5) {
      # filter for the particular department in the training data
      train_dept_ts <- train %>%
        filter(Dept == dept) %>%
        select(Store, Date, Weekly_Sales)
      
      # Reformat so that each column is a weekly time-series for that
      # store's department.
      # The dataframe has a shape (num_train_dates, num_stores)
      train_dept_ts <- train_frame %>%
        left_join(train_dept_ts, by = c('Date', 'Store')) %>%
        spread(Store, Weekly_Sales)
      
      # We create a similar dataframe to hold the forecasts on
      # the dates in the testing window
      test_dept_ts <- test_frame %>%
        mutate(Weekly_Sales = 0) %>%
        spread(Store, Weekly_Sales)
      
      ###### Model Fitting / Forecasting ######
      
      # naive forecast
      f_linear <- linear_model(train_dept_ts, test_dept_ts)
      f_linear <- shift(f_linear)
      test_month <- update_forecast(test_month, f_linear, dept, 3)
    }
    else {
      # filter for the particular department in the training data
      train_dept_ts <- train %>%
        filter(Dept == dept) %>%
        select(Store, Date, Weekly_Sales)
      
      # Reformat so that each column is a weekly time-series for that
      # store's department.
      # The dataframe has a shape (num_train_dates, num_stores)
      train_dept_ts <- train_frame %>%
        left_join(train_dept_ts, by = c('Date', 'Store')) %>%
        spread(Store, Weekly_Sales)
      
      # We create a similar dataframe to hold the forecasts on
      # the dates in the testing window
      test_dept_ts <- test_frame %>%
        mutate(Weekly_Sales = 0) %>%
        spread(Store, Weekly_Sales)
      
      ###### Model Fitting / Forecasting ######
      
      # naive forecast
      f_linear <- linear_model(train_dept_ts, test_dept_ts)
      test_month <- update_forecast(test_month, f_linear, dept, 3)
    }
    
    
  }
  
  # update global test dataframe
  update_test(test_month)


}


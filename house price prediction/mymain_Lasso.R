rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

train = read.csv('train.csv')
test = read.csv('test.csv')
train[which(train$Gr_Liv_Area > 4000), c('Sale_Price', 'PID', 'Gr_Liv_Area')]
train = train[-which(train$Gr_Liv_Area > 4500), ]
test_label = test$PID
test$Sale_Price = NA
data = rbind(train, test)







#Standard for the whole data set
Qualities = c('No' = 0, 'Poor' = 1, 'Fair' = 2, 'Typical' = 3, 'Good' = 4, 'Excellent' = 5)
#Check all categorical variables, find odinal variables. change all the quality levels
# table(data$Heating_QC)
data$Heating_QC = as.integer(factor(data$Heating_QC, levels = c('Poor', 'Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
# table(data$Heating_QC)

# table(data$Pool_QC)
data$Pool_QC = as.integer(factor(data$Pool_QC, levels = c('No_Pool', 'Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
data$Pool_QC[data$Pool_QC == 1] = data$Pool_QC[data$Pool_QC == 1] - 1
# table(data$Pool_QC)

# table(data$Fireplace_Qu)
data$Fireplace_Qu = as.integer(factor(data$Fireplace_Qu, levels = c('No_Fireplace', 'Poor', 'Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
data$Fireplace_Qu = data$Fireplace_Qu - 1
# table(data$Fireplace_Qu)


# table(data$Garage_Cond)
data$Garage_Cond = as.integer(factor(data$Garage_Cond, levels = c('No_Garage', 'Poor', 'Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
data$Garage_Cond = data$Garage_Cond - 1
# table(data$Garage_Cond)


# table(data$Garage_Qual)
data$Garage_Qual = as.integer(factor(data$Garage_Qual, levels = c('No_Garage', 'Poor', 'Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
data$Garage_Qual = data$Garage_Qual - 1
# table(data$Garage_Qual)

# table(data$Bsmt_Qual)
data$Bsmt_Qual = as.integer(factor(data$Bsmt_Qual, levels = c('No_Basement', 'Poor', 'Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
data$Bsmt_Qual = data$Bsmt_Qual - 1
# table(data$Bsmt_Qual)


# table(data$Bsmt_Cond)
data$Bsmt_Cond = as.integer(factor(data$Bsmt_Cond, levels = c('No_Basement', 'Poor', 'Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
data$Bsmt_Cond = data$Bsmt_Cond - 1
# table(data$Bsmt_Cond)


# table(data$Kitchen_Qual)
data$Kitchen_Qual = as.integer(factor(data$Kitchen_Qual, levels = c('Poor', 'Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
# table(data$Kitchen_Qual)

# table(data$Exter_Qual)
data$Exter_Qual = as.integer(factor(data$Exter_Qual, levels = c('Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
data$Exter_Qual = data$Exter_Qual + 1
# table(data$Exter_Qual)

# table(data$Exter_Cond)
data$Exter_Cond = as.integer(factor(data$Exter_Cond, levels = c('Poor', 'Fair', 'Typical', 'Good', 'Excellent'), ordered = TRUE))
# table(data$Exter_Cond)

#table(data$Overall_Qual)
# data$Overall_Qual = as.integer(factor(data$Overall_Qual), levels = c('Very_Poor', 'Poor', 'Fair', 'Below_Average', 'Average', 'Above_Average', 'Good',
#                                                                      'Very_Good', 'Excellent', 'Very_Excellent'), ordered = TRUE)


data$Total_Bathrooms = data$Full_Bath + (data$Half_Bath*0.5) + data$Bsmt_Full_Bath + (data$Bsmt_Half_Bath*0.5)
data$Remod = ifelse(data$Year_Built==data$Year_Remod_Add, 0, 1) #0=No Remodeling, 1=Remodeling
data$Age = as.numeric(data$Year_Sold)-data$Year_Remod_Add
data$Total_Sq_Feet = data$Gr_Liv_Area + data$Total_Bsmt_SF






#Create a new variable about neighbour
#'Northridge_Heights', 'Stone_Brook', 'Northridge' have relatively high sale price; 'Meadow_Village', 'Briardale', 'Iowa_Dot_and_Rail_Road' have relatively low sale price
data$NeighRich[data$Neighborhood %in% c('Stone_Brook', 'Northridge', 'Northridge_Heights')] = 'Rich'
data$NeighRich[!data$Neighborhood %in% c('Stone_Brook', 'Northridge', 'Northridge_Heights', 'Meadow_Village', 'Briardale', 'Iowa_Dot_and_Rail_Road')] <- 'Medium'
data$NeighRich[data$Neighborhood %in% c('Meadow_Village', 'Briardale', 'Iowa_Dot_and_Rail_Road')] <- 'Poor'
data$NeighRich = as.factor(data$NeighRich)

#drop some variables
data = data[, !names(data) %in% c('Street', 'Utilities', 'Land_Slope', 'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Three_season_porch', 'Pool_Area', 'Misc_Val', 'Longitude', 'Latitude', 'Garage_Yr_Blt')]



#data
#seperate numeric and factor
is.fact = sapply(data, is.factor)
factors.df = data[, is.fact]
numeric.df = data[, !is.fact]


dmy = model.matrix(~.-1, factors.df)
data = cbind(numeric.df, dmy)

train = data[-which(is.na(data$Sale_Price)), ]
test = data[which(is.na(data$Sale_Price)), ]

train_price = log(train$Sale_Price)
train = within(train, rm('Sale_Price'))

train$PID = NULL
test$PID = NULL
test$Sale_Price = NULL

one_step_lasso = function(r, x, lam){
  xx = sum(x^2)
  xr = sum(r*x)
  b = (abs(xr) -lam/2)/xx
  b = sign(xr)*ifelse(b>0, b, 0)
  return(b)
}

mylasso = function(X, y, lam, n.iter = 500, standardize  = TRUE)
{
  # X: n-by-p design matrix without the intercept
  # y: n-by-1 response vector
  # lam: lambda value
  # n.iter: number of iterations
  # standardize: if True, center and scale X and y. 
  p=dim(X)[2]
  x_mean=matrix(nrow=1,ncol=p)
  x_sd=matrix(nrow=1,ncol=p)
  
  
  
  # YOUR CODE
  # If standardize  = TRUE, center and scale X and Y
  if (standardize==T)
  {
    for (i in 1:p)
    {
      x_mean[i]=mean(X[,i])
      x_sd[i]=sd(X[,i])
    }
    
    y_mean=mean(y)
    y_sd=sd(y)
    X=scale(X)
    y=scale(y,scale=FALSE)
    
    X[is.na(X)]=0
    
  }
  
  
  
  # Initial values for residual and coefficient vector b
  b = rep(0, p)
  r = y
  
  for(step in 1:n.iter){
    for(j in 1:p){
      
      # YOUR CODE 
      
      # 1) Update the residual vector  
      # r <-- r + X[, j] * b[j]
      # r on the left: residual in blue on p37 of [lec_W3_VariableSelection.pdf]
      # r on the right: current residual
      
      r=r + X[, j] * b[j]
      # 2) Apply one_step_lasso to update beta_j
      b[j] = one_step_lasso(r, X[, j], lam)
      
      # 3) Update the current residual vector
      r = r - X[, j] * b[j]
      
      
    }
    
  }
  
  b=b/x_sd
  b[is.na(b)]=0
  b0=y_mean-b%*%t(x_mean)
  # YOUR CODE: scale back b and add intercept b0
  # For b0, check p13 of [lec_W3_VariableSelection.pdf]. 
  return(c(b0, b))
}

b = mylasso(train, train_price, 13, n.iter = 1000)
pred = rep(0, nrow(test))
for (i in 1:nrow(test)) {
  pred[i] = sum(b[-1]*test[i,]) + b[1]
}
df = data.frame(test_label, pred)
colnames(df) = c('PID', 'Sale_Price')
write.table(df, file = 'mysubmission3.txt', quote = FALSE, row.names = FALSE, sep = ',')





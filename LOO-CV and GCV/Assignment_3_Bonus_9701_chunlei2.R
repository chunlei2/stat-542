rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
mydata = read.csv('Coding3_Bonus_Data.csv')
lo.lev <- function(x1, sp){
  
  ## YOUR CODE: compute the diagonal entries of the smoother
  ##             matrix S, stored in vector "lev"
  ## Tip: check how we compute the smoother matrix
  ##      for smoothing spline models
  n = length(x1)
  A = matrix(0, n, n)
  for (i in 1:n) {
    y = rep(0, n)
    y[i] = 1
    y_i = loess(y ~ x1, span = sp, control = loess.control(surface = "direct"))$fitted
    # mod = loess(y ~ x1, span = sp)
    # y_i = predict(mod, mydata)
    A[, i] = y_i
  }
  lev = diag(A)
  return(lev)
}

onestep_CV <- function(x1, y1, sp){
  
  ## YOUR CODE: 
  ## 1) fit a loess model y1 ~ x1 with span = sp, and extract 
  ##    the corresponding residual vector
  ## 2) call lo.lev to obtain the diagonal entries of S
  ## 3) compute LOO-CV and GCV using formula from lecture notes
  ##    [lec_W5_NonlinearRegression.pdf] page 33. 
  res = loess(y1 ~ x1, span = sp, control = loess.control(surface = "direct"))$residuals
  s = lo.lev(x1, sp)
  cv = mean((res/(1-s))^2)
  gcv = mean((res/(1-mean(s)))^2)
  return(list(cv = cv, gcv = gcv))
}

myCV <- function(x1, y1, span){
  ## x1, y1: two vectors
  ## span: a sequence of values for "span"
  
  m = length(span)
  cv = rep(0, m)
  gcv = rep(0, m)
  for(i in 1:m){
    tmp = onestep_CV(x1, y1, span[i])
    cv[i] = tmp$cv
    gcv[i] = tmp$gcv
  }
  return(list(cv = cv, gcv = gcv))
}

span1 = seq(from = 0.2, by = 0.05, length = 15 )
cv.out = myCV(mydata$x, mydata$y, span1)
cv = cv.out$cv
gcv = cv.out$gcv
cbind(cv, gcv)



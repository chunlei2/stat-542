rm(list = ls())
library(HMM)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

data = read.csv("Coding3_HMM_Data.csv")
dim(data)

head(data)

mz=2; mx=3
ini.A = matrix(1, mz, mz)
ini.A = ini.A/rowSums(ini.A)
ini.B = matrix(1:6, mz, mx)
ini.B = ini.B/rowSums(ini.B)
ini.w = c(1/2, 1/2)


forward.prob = function(x, A, B, w){
  
  # Output the forward probability matrix alp 
  # alp: T by mz, (t, i) entry = P(x_{1:t}, Z_t = i)
  
  T = length(x)
  mz = nrow(A)
  alp = matrix(0, T, mz)
  
  # fill in the first row of alp
  alp[1, ] = w*B[, x[1]]
  
  # Recursively compute the remaining rows of alp
  for(t in 2:T){
    tmp = alp[t-1, ] %*% A
    alp[t, ] = tmp * B[, x[t]]
  }
  return(alp)
}

backward.prob = function(x, A, B, w){
  # Output the backward probability matrix beta
  # beta: T by mz, (t, i) entry = P(x_{(t+1):n} | Z_t = i)
  # for t=1, ..., n-1
  
  T = length(x)
  mz = nrow(A)
  beta = matrix(1, T, mz)
  
  # The last row of beta is all 1.
  # Recursively compute the previous rows of beta
  for(t in (T-1):1){
    tmp = as.matrix(beta[t+1, ] * B[, x[t+1]])  # make tmp a column vector
    beta[t, ] = t(A %*% tmp)
  }
  return(beta)
}


BW.onestep = function(x, A, B, w){
  # Input: 
  # x: T-by-1 observation sequence
  # A: current estimate for mz-by-mz transition matrix
  # B: current estimate for mz-by-mx emission matrix
  # w: current estimate for mz-by-1 initial distribution over Z_1
  # Output the updated parameters
  # para = list(A = A1, B = B1)
  
  # We DO NOT update the initial distribution w
  
  T = length(x)
  mz = nrow(A)
  alp = forward.prob(x, A, B, w) #T, mz
  beta = backward.prob(x, A, B, w) #T, mz
  myGamma = array(0, dim=c(mz, mz, T-1))
  
  ###
  ## YOUR CODE: 
  ## Compute gamma_t(i,j), which are stored in myGamma
  ##
  
  for (t in 1:(T-1)) {
    for (i in 1:mz) {
      for (j in 1:mz) {
        myGamma[i, j, t] = alp[t, i]*beta[t+1, j]*B[j, x[t+1]]*A[i,j]
      }
    }
  }
  
  
  A = rowSums(myGamma, dims = 2)
  A = A/rowSums(A)
  
  tmp = apply(myGamma, c(1, 3), sum)  # mz-by-(T-1)
  tmp = cbind(tmp, colSums(myGamma[, , T-1]))
  for(l in 1:mx){
    B[, l] = rowSums(tmp[, which(x==l)])
  }
  B = B/rowSums(B)
  return(list(A = A, B = B))
}

myBW = function(x, A, B, w, n.iter = 100){
  # Input: 
  # x: T-by-1 observation sequence
  # A: initial estimate for mz-by-mz transition matrix
  # B: initial estimate for mz-by-mx emission matrix
  # w: initial estimate for mz-by-1 initial distribution over Z_1
  # Output MLE of A and B; we do not update w
  # list(A = A, B=B, w = w)
  
  for(i in 1:n.iter){
    update.para = BW.onestep(x, A, B, w)
    A = update.para$A
    B = update.para$B
  }
  return(list(A = A, B = B, w = w))
}

myViterbi = function(X, A, B, w){
  T = length(X)
  delta = matrix(0, T, mz)
  Z = rep(NA, T)
  logA = log(A)
  logB = log(B)
  logw = log(w)
  delta[1, ] = logw + logB[, X[1]]
  # delta[1, ] = w*B[, X[1]]
  for (t in 2:T) {
    for (i in 1:mz) {
      # delta[t, i] = max()
      delta[t, i] = max(delta[t-1, ] + logA[, i]) + logB[i, X[t]]
      # delta[t, i] = max(delta[t-1, ]*A[, i])*B[i, X[t]]
    }
  }
  
  for (t in rev(1:T)) {
    if (t == T) {
      Z[t] = which.max(delta[t, ])
    }
    else{
      Z[t] = which.max(delta[t, ] + logA[, Z[t+1]])
    }
  }
  Z
}

myout = myBW(data$X, ini.A, ini.B, ini.w, n.iter = 100)
myout.Z = myViterbi(data$X, myout$A, myout$B, ini.w)
for (i in 1:500) {
  if (myout.Z[i] == 1) {
    myout.Z[i] = 'A'
  }
  else{
    myout.Z[i] = 'B'
  }
}
write.table(myout.Z, file = "Coding3_HMM_Viterbi_Output.txt", 
            row.names = FALSE, col.names = FALSE)
hmm0 =initHMM(c("A", "B"), c(1, 2, 3), 
              startProbs = ini.w,
              transProbs = ini.A, emissionProbs = ini.B)

true.out = baumWelch(hmm0, data$X, maxIterations=100, pseudoCount=0)
true.out$hmm

true.viterbi = viterbi(true.out$hmm, data$X)
write.table(true.viterbi, file = "Coding3_HMM_True_Viterbi_Output.txt",
           row.names = FALSE, col.names = FALSE)
sum(true.viterbi != myout.Z)

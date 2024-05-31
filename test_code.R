for (s in 1:total_S){
  # NIW posterior parameters
  #前面一堆 53X53
  p=1
  N=2
  
  Y_simulation = (simulation_data[(p+1):nrow(simulation_data),]) #contains the obs of the two variables and moves first obs
  Y = (simulation_data[(p+1):nrow(simulation_data),])
  X_simulation = matrix(1,nrow(Y_simulation),1) #initializes the X matrix with a column of ones(intercept) in the VAR model.                                      
  # adds the lagged values of the two variables to the X matrix, in this case, it adds one lagged value for each of the two variables.
  for (i in 1:p){
    X_simulation     = cbind(X_simulation, (simulation_data[(p+1):nrow(simulation_data)-i,]))
  }
  
  Y_simulation = as.matrix(Y_simulation)
  
  X_simulation = as.matrix(X_simulation)
  X = X_simulation
  Y = Y_simulation
  N           = ncol(Y_simulation)                          
  p           = frequency(Y_simulation)
  lambda.posterior  = matrix(NA, S1+S2, 1)
  
  # set the initial value of lambda
  lambda.posterior[1] = 5    
  s = 1
  V.bar.inv              = t(X)%*%X/lambda.posterior[s] + diag(1/diag(V.prior)) 
  V.bar                  = solve(V.bar.inv)
  A.bar                  = V.bar%*%(t(X)%*%Y/lambda.posterior[s] + diag(1/diag(V.prior))%*%A.prior)
  nu.bar                 = nrow(Y) + nu.prior
  S.bar                  = S.prior + t(Y)%*%Y/lambda.posterior[s] +t(A.prior)%*%diag(1/diag(V.prior))%*%A.prior - t(A.bar)%*%V.bar.inv%*%A.bar
  S.bar.inv              = solve(S.bar)
  
  # posterior draws for A and Sigma
  Sigma.posterior.IW     = rWishart(1, df=nu.bar, Sigma=S.bar.inv)
  Sigma.posterior.draw   = solve(Sigma.posterior.IW[,,1])
  Sigma.posterior[,,s]   = Sigma.posterior.draw
  A.posterior[,,s]       = array(rnorm(prod(c(dim(A.bar),1))),c(dim(A.bar),1))
  L                      = t(chol(V.bar))
  A.posterior[,,s]       = A.bar + L%*%A.posterior[,,s]%*%chol(Sigma.posterior[,,s])
  
  c                      = -nrow(Y)*ncol(Y)/2+1          # N=13
  diff_A                 = Y - X%*%A.posterior[,,s]
  product                = t(diff_A) %*% diff_A
  b                      = tr(solve(Sigma.posterior[,,s]) %*% product)
  a                      = 2 / lambda.priors$alhpa
  
  # Draw next period value for lambda from GIG distribution
  if (s!=total_S){
    lambda.posterior[s+1] = GIGrvg::rgig(n=1, lambda = c, chi = b, psi = a)
  }
}

output                 = list(A.posterior.exten = A.posterior[,,(S1+1):S2], 
                              Sigma.posterior.exten = Sigma.posterior[,,(S1+1):S2], 
                              lambda.posterior.exten = lambda.posterior[(S1+1):S2,])
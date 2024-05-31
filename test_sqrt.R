library(Rmpfr)
library(MASS)
library(dplyr)
library(psych)
S1= 5
S2= 95
total_S = S1+S2
S=100
set.seed(123)
n <- 1000  # Number of observations
mu <- 0    # Mean
sigma <- 1 # Standard deviation
simulation_data <- data.frame(RW1 = cumsum(rnorm(n, mu, sigma)),RW2 = cumsum(rnorm(n, mu, sigma)))

p=1
N=2

Y_simulation = (simulation_data[(p+1):nrow(simulation_data),]) #contains the obs of the two variables and moves first obs
X_simulation = matrix(1,nrow(Y_simulation),) #initializes the X matrix with a column of ones(intercept) in the VAR model.                                      
# adds the lagged values of the two variables to the X matrix, in this case, it adds one lagged value for each of the two variables.
for (i in 1:p){
  X_simulation     = cbind(X_simulation, (simulation_data[(p+1):nrow(simulation_data)-i,]))
}

Y_simulation = as.matrix(Y_simulation)
X_simulation = as.matrix(X_simulation)

Y<- Y_simulation
X<- X_simulation

#posterior_t <- function(Y, X, S){

A.hat       = solve(t(X)%*%X)%*%t(X)%*%Y
Sigma.hat   = t(Y-X%*%A.hat)%*%(Y-X%*%A.hat)/nrow(Y)

N = ncol(Y)
T <- NROW(Y)
kappa.1   = 0.02^2
kappa.2   = 100
K = 1 + (p*N)

A.prior     = matrix(0, K , N)
A.prior[2:(N+1),] = diag(N)
V.prior     = diag(c(kappa.2,kappa.1*((1:p)^(-2))%x%rep(1,N)))
S.prior     = diag(diag(Sigma.hat))
nu.prior    = N+1


alpha <- 1
lambda.0 <- rexp(nrow(Y), rate = 1/alpha)
lambda.priors = list(alpha = 5)

omega.0 = diag(lambda.0)

# Initialize arrays to store posterior draws
Sigma.posterior.draws = array(NA, c(N,N,S))
A.posterior.draws = array(NA, c((1+p*N),N,S))

Omega.posterior.draws = array(NA, c(T,T,S+1))
Omega.posterior.draws[,,1] = omega.0

lambda.posterior.draws = array(NA,c(T,S))
b = array(NA,c(T,S))

#lambda.posterior.draws <- array(NA,c(T,S+1))

for (s in 1:S){
# for (s in 1:S){
#   if (s == 1) {
#     lambda.s = lambda.0
#   } else {
#     lambda.s    = lambda.posterior.draws[,s]
#   }
#   
#   Omega = (diag(lambda.s))
#   Omega.inv = diag(1/lambda.s)
#  # Omega.inv = sqrt(diag(1/lambda.s))
  
  Omega.inv = solve(Omega.posterior.draws[,,s])
  Omega.inv = sqrt(Omega.inv)
  
  V.bar.ext       = solve(t(X)%*% Omega.inv%*%X + solve(V.prior))
  A.bar.ext       = V.bar.ext%*%(t(X)%*% Omega.inv%*%Y + solve(V.prior)%*%A.prior)
  nu.bar.ext      = T + nu.prior
  S.bar.ext       = S.prior + t(Y)%*% Omega.inv%*%Y + t(A.prior)%*%solve(V.prior)%*%A.prior - t(A.bar.ext)%*%solve(V.bar.ext)%*%A.bar.ext
  S.bar.ext.inv   = solve(S.bar.ext)
  
  
  Sigma.inv.draw[,,s] = rWishart(1, df = nu.bar.ext, Sigma = S.bar.ext.inv)[,,1]
  Sigma.posterior.draws[,,s] = solve(Sigma.inv.draw[,,s])
  A.posterior.draws[,,s] = matrix(mvtnorm::rmvnorm(1, mean=as.vector(A.bar.ext), sigma = Sigma.posterior.draws[,,s] %x% V.bar.ext), ncol=N)
  
  # c                      = -N/ 2 + 1          # N=13
  # diff_A                 = Y- X%*%A.posterior.draws[,,s]
  # product                = t(diff_A) %*% diff_A
  # b                      = tr(Sigma.inv.draw %*% product)
  # a                      = 2 / lambda.priors$alpha
  # 
  # lambda = GIGrvg::rgig(T, lambda = c, chi = b, psi = a)
  # lambda.posterior.draws[,s+1] <- lambda
  
 }
  
  u.t = Y-X%*%A.posterior.draws[,,s]
  #    ---- loop lambda posterior ----   #
  for (x in 1:T){
  c                      = -N/2 + 1          # N=13
  b[,s]                  = t((u.t)[x,])%*%Sigma.posterior.draws[,,s]%*%(u.t)[x,]
  a                      = 2 / lambda.priors$alpha
}
  
  for (x in 1:T){

  lambda.posterior.draws[x,s] = GIGrvg::rgig(1, lambda = c, chi = b[x,s], psi = a)
  }  
  Omega.posterior.draws[,,s+1] = diag(lambda.posterior.draws[,s])
#}

pris = (list(A.posterior.draws = A.posterior.draws, 
             Sigma.posterior.draws = Sigma.posterior.draws,
             lambda.posterior.draws=lambda.posterior.draws))

#}


stats = posterior_t(Y, X, 1, 100)

round(apply(pris$Sigma.posterior.draws, 1:2, mean),2)

round(apply(stats[[2]], 1:2, mean),2)

round(mean(stats[[3]]),2)

round(apply(stats[[2]], 1:2, mean),2) * round(mean(stats[[3]]),2)


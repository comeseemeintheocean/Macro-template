library(Rmpfr)
library(MASS)
library(dplyr)
library(psych)
S1= 100
S2= 1000
total_S = S1+S2
S=1000
set.seed(12)
n <- 1000  # Number of observations
mu <- 0    # Mean
sigma <- 1 # Standard deviation
simulation_data <- data.frame(RW1 = cumsum(rnorm(n, mu, sigma)),RW2 = cumsum(rnorm(n, mu, sigma)))

p=1
N=2

Y_simulation = (simulation_data[(p+1):nrow(simulation_data),]) #contains the obs of the two variables and moves first obs
X_simulation = matrix(1,nrow(Y_simulation),) #initializes the X matrix with a column of ones(intercept) in the VAR model.                                      

for (i in 1:p){
  X_simulation     = cbind(X_simulation, (simulation_data[(p+1):nrow(simulation_data)-i,]))
}

Y_simulation = as.matrix(Y_simulation)
X_simulation = as.matrix(X_simulation)

Y<- Y_simulation
X<- X_simulation

posterior_t <- function(Y, X, S){

A.hat       = solve(t(X)%*%X)%*%t(X)%*%Y
Sigma.hat   = t(Y-X%*%A.hat)%*%(Y-X%*%A.hat)/nrow(Y)

N = ncol(Y)
T <- nrow(Y)
kappa.1   = 0.02^2
kappa.2   = 100
K = 1 + (p*N)

A.prior     = matrix(0, K , N)
A.prior[2:(N+1),] = diag(N)
V.prior     = diag(c(kappa.2,kappa.1*((1:p)^(-2))%x%rep(1,N)))
S.prior     = diag(diag(Sigma.hat))
nu.prior    = N+1

alpha <- 2
lambda.0 <- rexp(T, rate = 1/alpha)
lambda.priors = list(alpha = 2)


# Initialize arrays to store posterior draws
Sigma.posterior.draws = array(NA, c(N,N,S))
A.posterior.draws = array(NA, c((1+p*N),N,S))

lambda.posterior.draws = array(NA,c(T,S+1))
b = array(NA,c(T,S))

#lambda.posterior.draws <- array(NA,c(T,S+1))

for (s in 1:S){
  
  if (s == 1) {
    lambda.s = lambda.0
  } else {
    lambda.s    = lambda.posterior.draws[,s]
  }
  
  Omega = (diag(lambda.s))
  Omega.inv = diag(1/lambda.s)
  # Omega.inv = sqrt(diag(1/lambda.s))
  # Omega.inv = sqrt(Omega.inv)
  
  V.bar.inv.ext   = t(X)%*%Omega.inv%*%X + solve(V.prior)
  V.bar.ext       = solve(V.bar.inv.ext)
  A.bar.ext       = V.bar.ext%*%(t(X)%*%Omega.inv%*%Y + diag(1/diag(V.prior))%*%A.prior)
  nu.bar.ext      = T + nu.prior
  S.bar.ext       = S.prior + t(Y)%*%Omega.inv%*%Y + t(A.prior)%*%diag(1/diag(V.prior))%*%A.prior - t(A.bar.ext)%*%V.bar.inv.ext%*%A.bar.ext
  S.bar.ext.inv   = solve(S.bar.ext)
  
  
  Sigma.inv.draw = rWishart(1, df = nu.bar.ext, Sigma = S.bar.ext.inv)[,,1]
  Sigma.posterior.draws[,,s] = solve(Sigma.inv.draw)
  A.posterior.draws[,,s] = matrix(mvtnorm::rmvnorm(1, mean=as.vector(A.bar.ext), sigma = Sigma.posterior.draws[,,s] %x% V.bar.ext), ncol=N)
  
  
  u.t = Y-X%*%A.posterior.draws[,,s]
  #    ---- loop lambda posterior ----   #
  c                      = -N/2 + 1          # N=13
  a                      = 2 / lambda.priors$alpha
  for (x in 1:T){
    b                  = t((u.t)[x,])%*%Sigma.posterior.draws[,,s]%*%(u.t)[x,]
    lambda.posterior.draws[x,s+1] = GIGrvg::rgig(1, lambda = c, chi = b, psi = a)
  } # END x loop
} # END s loop


#}

pris = (list(A.posterior.draws = A.posterior.draws[,,S1:S],
Sigma.posterior.draws = Sigma.posterior.draws[,,S1:S],
lambda.posterior.draws = lambda.posterior.draws[,(S1+1):(S+1)]))

# pris = (list(A.posterior.draws = A.posterior.draws, 
#              Sigma.posterior.draws = Sigma.posterior.draws,
#              lambda.posterior.draws=lambda.posterior.draws))

}


stats = posterior_t(Y, X, 1000)

round(apply(stats$A.posterior.draws, 1:2, mean),2)
round(apply(stats$Sigma.posterior.draws, 1:2, mean),2)*median(stats$lambda.posterior.draws)

# round(apply(pris$Sigma.posterior.draws, 1:2, mean),2)


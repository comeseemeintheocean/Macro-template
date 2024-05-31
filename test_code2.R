S1= 5
S2= 95
total_S = S1+S2
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

N           = ncol(Y_simulation)                          
p           = frequency(Y_simulation)
A.hat       = solve(t(X_simulation)%*%X_simulation)%*%t(X_simulation)%*%Y_simulation
Sigma.hat   = t(Y_simulation-X_simulation%*%A.hat)%*%(Y_simulation-X_simulation%*%A.hat)/nrow(Y_simulation)

# Minnesota prior 
kappa.1       = 0.02^2                                    
kappa.2       = 100                                   
A.prior       = matrix(0,nrow(A.hat),ncol(A.hat))
A.prior[2:(N+1),] = diag(N) 
V.prior       = diag(c(kappa.2,kappa.1*((1:p)^(-2))%x%rep(1,N)))
S.prior       = diag(diag(Sigma.hat))
nu.prior      = N+1


A.posterior       = array(NA, dim = c((1+N*p),N,total_S))
Sigma.posterior   = array(NA, dim = c(N,N,total_S))
lambda.posterior  = array(NA, dim = c(nrow(Y),1,total_S))

# set the initial value of lambda
lambda.posterior[,,1] = 5                      


# parameter alpha
lambda.priors = list(alpha = 1.5)
#posterior.draws.extended = function (total_S, Y, X){
  for (s in 1:total_S){
    
    # NIW posterior parameters
    # 53X53
    V.bar.inv              = t(X)%*%X/lambda.posterior[,,s] + diag(1/diag(V.prior)) 
    V.bar                  = solve(V.bar.inv)
    A.bar                  = V.bar%*%(t(X)%*%Y/lambda.posterior[,,s] + diag(1/diag(V.prior))%*%A.prior)
    nu.bar                 = nrow(Y) + nu.prior
    S.bar                  = S.prior + t(Y)%*%Y/lambda.posterior[,,s] +t(A.prior)%*%diag(1/diag(V.prior))%*%A.prior - t(A.bar)%*%V.bar.inv%*%A.bar
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
  #  if (s!=total_S){
  #    lambda.posterior[,,s+1] = array(GIGrvg::rgig(n=nrow(Y), lambda = c, chi = b, psi = a), dim = nrow(Y))
   # }
  }
  



####################################################################################

# BVAR_MA_fit <- function(data, p, S1, S2, kappa1, kappa2, A_prior, V_prior, S_prior, nu_prior, N){
#   Y = (data[(p+1):nrow(data),])
#   N = ncol(Y)
#   Ty = nrow(Y)
#   S = S1 + S2
#   X = matrix(1,nrow(Y),1) 
#   K = 1+p*N
#   tau = 0.15
#   for (i in 1:p){
#     X     = cbind(X, (data[(p+1):nrow(data)-i,]))
#   }
#   X = as.matrix(X)
#   Y = as.matrix(Y)
#   
#   nu_bar = Ty + nu_prior
#   psi = 0
#   H_psi = matrix(0, nrow = Ty, ncol = Ty)
#   O_psi = diag(rep(1, Ty))
#   O_psi[1,1] = 1+psi^2
#   for (t in 1:Ty){
#     H_psi[t,t] = 1
#     if (t < Ty){
#       H_psi[t+1, t] = psi
#     }
#   }
#   A_posterior = array(rnorm(prod(c(c(K,N),S))),c(c(K,N),S))
#   Sigma_posterior = array(dim = c(N,N,S))
#   Omega_posterior = array(dim = c(Ty, Ty, S+1))
#   Omega_posterior[,,1] = H_psi %*% O_psi %*% t(H_psi)
#   lambda.posterior  = matrix(NA, total_S, 1)
#   # set the initial value of lambda
#   lambda.posterior[1] = 5                      
#   lambda.priors = list(alpha = 1.5)
#   
#   accept = 0
#   pb <- progress_bar$new(total = S)
#   
#   for (s in 1:S){
#     pb$tick()
#     
#     chol_Omega = chol(Omega_posterior[,,s]) #########change 
#     chol_Omega_inv = solve(chol_Omega)
#     #V_bar = solve(t(X)%*%X + solve(V_prior))
#     V_bar = solve(t(chol_Omega_inv%*%X)%*%(chol_Omega_inv%*%X) + solve(V_prior))
#     L = t(chol(V_bar))
#     #A_bar = V_bar%*%(t(X)%*%Y + solve(V_prior)%*%A_prior)
#     A_bar = V_bar%*%(t(chol_Omega_inv%*%X)%*%(chol_Omega_inv%*%Y) + solve(V_prior)%*%A_prior)
#     #S_bar = S_prior +  t(Y)%*%Y + t(A_prior)%*%solve(V_prior)%*%A_prior - t(A_bar)%*%solve(V_bar)%*%A_bar
#     S_bar = S_prior +  t(chol_Omega_inv%*%Y)%*%(chol_Omega_inv%*%Y) + t(A_prior)%*%solve(V_prior)%*%A_prior - t(A_bar)%*%solve(V_bar)%*%A_bar
#     S_bar_inv = solve(S_bar)
#     Sigma_posterior[,,s] = solve(rWishart(1, df=nu_bar, Sigma=S_bar_inv)[,,1])
#     A_posterior[,,s]= A_bar + L%*%A_posterior[,,s]%*%chol(Sigma_posterior[,,1]) ############change 
#     ###############MH Step    ##################################
#       
#       if (s!=total_S){
#         c                      = -nrow(Y)*ncol(Y)/2+1          # N=13
#         diff_A                 = Y - X%*%A_posterior[,,s]
#         product                = t(diff_A) %*% diff_A
#         b                      = tr(solve(Sigma_posterior[,,s]) %*% product)
#         a                      = 2 / lambda.priors$alhpa
#         lambda.posterior[s+1] = GIGrvg::rgig(n=1, lambda = c, chi = b, psi = a)
#       }
#       
#       
#       Omega_draw = H_psi %*% O_psi %*% t(H_psi)
#       chol_Omega_draw = chol(Omega_draw)
#       chol_Omega_draw_inv = solve(chol_Omega_draw)
#       Sigma_posterior_inv = solve(Sigma_posterior[,,s])
#       #p_y_numerator = ((1 + psi_candidate^2)/( 1 + psi^2))^(-N/2)*exp(-1/2*tr(solve(Sigma_posterior[,,s])%*%t(chol_Omega_inv%*%(Y-X%*%A_posterior[,,s]))%*%(chol_Omega_inv%*%(Y-X%*%A_posterior[,,s])))
#       #                                                                +1/2*tr(solve(Sigma_posterior[,,s])%*%t(chol_Omega_draw_inv%*%(Y-X%*%A_posterior[,,s]))%*%(chol_Omega_inv%*%(Y-X%*%A_posterior[,,s]))\
#       
#       Likelihood_ratio = ((1+psi_candidate^2)/(1+psi^2))^(N/2) * exp(-1/2*(tr(
#         Sigma_posterior_inv%*%t(chol_Omega_draw_inv%*%(Y-X%*%A_posterior[,,s]))%*%(chol_Omega_draw_inv%*%(Y-X%*%A_posterior[,,s]))
#       ) - tr(
#         Sigma_posterior_inv%*%t(chol_Omega_inv%*%(Y-X%*%A_posterior[,,s]))%*%(chol_Omega_inv%*%(Y-X%*%A_posterior[,,s]))))
#       )
#       prior_ratio = dtruncnorm(x = psi_candidate, mean = psi_prior, sd = Vpsi_prior)/dtruncnorm(x = psi, mean = psi_prior, sd = Vpsi_prior)
#       proposal_ratio = dnorm(x = psi, mean = psi_candidate, sd = tau)/dnorm(x=psi_candidate, mean = psi, sd = tau)
#       #numerator = p_y_numerator * dtruncnorm(x=psi_candidate, mean=psi_prior, sd = Vpsi_prior, a = -1, b = 1) *dnorm(x = psi, mean = psi_candidate, sd = tau)
#       #denominator =  dtruncnorm(x=psi, mean=psi_prior, sd = Vpsi_prior, a = -1, b = 1) *dnorm(x = psi_candidate, mean = psi, sd = tau)
#       #alpha = min(1, numerator/denominator)
#       alpha = min(1, Likelihood_ratio, prior_ratio, proposal_ratio)
#     }
#     u = runif(1, min = 0, max = 1)
#     #print(numerator/denominator)
#     if (u <= alpha){
#       accept = accept + 1
#       psi = psi_candidate
#       Omega_posterior[,,s+1] = Omega_draw
#     }
#     else{
#       Omega_posterior[,,s+1] = Omega_posterior[,,s]}
#     ##################################################    #####
#   }
#   A_posterior = A_posterior[,,(S1+1):(S1+S2)]
#   Sigma_posterior = Sigma_posterior[,,(S1+1):(S1+S2)]
#   Omega_posterior = Omega_posterior[,,(S1+2):(S1+S2+1)]
#   print("acceptance rate of the Metropolis Hasting Step: ")
#   print(accept/S)
#   return(list(A_posterior, Sigma_posterior, Omega_posterior))
# }

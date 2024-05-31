# extended + forecasting

### Proof of extended model
The Gibbs sampler method will be applied to generate random draws from the full conditional posterior distribution:
  
  1. Draw $\Sigma^{(s)}$ from the $IW(\bar{S},\bar{\nu})$ distribution.
2. Draw $A^{(s)}$ from the $MN(\bar{A},\Sigma^{(s)}, \bar{V})$ distribution.
3. Draw $\lambda_t^{(s)}$ from $GIG(a,b,p)$.

Repeat steps 1, step 2 and 3 for $S_1$+$S_2$times.

Discard the first draws that allowed the algorithm to converge to the stationary posterior distribution.

Output is $\left\{ {A^{(s)}, \Sigma^{(s)}}, \lambda_t^{(s)}\right\}^{S_1+S_2}_{s=S_1+1}$.

```{r}
# setup 
S1                = 5000                         # will be discard
S2                = 45000                          
total_S           = S1+S2
A.posterior       = array(NA, dim = c((1+N*p),N,S1+S2))
Sigma.posterior   = array(NA, dim = c(N,N,S1+S2))
lambda.posterior  = matrix(NA, S1+S2, 1)

alpha <- 5 
lambda.0 <- rexp(1, rate = 1/alpha)
# set the initial value of lambda
lambda.posterior[1] = lambda.0                            

# parameter alpha
lambda.priors = list(alpha = 5)
```

```{r}
#| echo: false
#| message: false
#| warning: false

posterior.draws.extended = function (total_S, Y, X){
  for (s in 1:total_S){
    
    V.bar.inv              = t(X)%*%X + diag(1/diag(lambda.posterior[s]* V.prior)) 
    V.bar                  = solve(V.bar.inv)
    A.bar                  = V.bar%*%(t(X)%*%Y + diag(1/diag(lambda.posterior[s]* V.prior))%*%A.prior)
    nu.bar                 = nrow(Y) + nu.prior
    S.bar                  = S.prior + t(Y)%*%Y + t(A.prior)%*%diag(1/diag(lambda.posterior[s]* V.prior))%*%A.prior - t(A.bar)%*%V.bar.inv%*%A.bar
    S.bar.inv              = solve(S.bar)
    
    Sigma.posterior.IW     = rWishart(1, df=nu.bar, Sigma=S.bar.inv)
    Sigma.posterior.draw   = apply(Sigma.posterior.IW,3,solve)
    Sigma.posterior[,,s]   = Sigma.posterior.draw
    A.posterior[,,s]       = array(rnorm(prod(c(dim(A.bar),1))),c(dim(A.bar),1))
    L                      = t(chol(V.bar))
    A.posterior[,,s]       = A.bar + L%*%A.posterior[,,s]%*%chol(Sigma.posterior[,,s])
    
    a                             = 2 / lambda.priors$alpha
    deviation_matrix              = A.posterior[,,s] - A.prior
    weighted_deviation_squared    = t(deviation_matrix) %*% solve(V.prior) %*% deviation_matrix
    b                             = sum(diag(solve(Sigma.posterior[,,s] %*% weighted_deviation_squared)))
    p                             = - (T * N) / 2 + 1            
    
    if (s!=total_S){
      lambda.posterior[s+1] = GIGrvg::rgig(n=1, lambda = p, chi = b, psi = a)
    }
  }
  output  = list (A.posterior.exten = A.posterior, 
                  Sigma.posterior.exten = Sigma.posterior, 
                  lambda.posterior.exten = lambda.posterior)
  return(output)
}
```

################may not use########### test///////
```{r}
posterior.draws.extended = function (total_S, Y, X){
  for (s in 1:total_S){
    # NIW posterior parameters
    # 53X53
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
    
    #   Sigma.posterior   = rWishart(1, df=nu.bar, Sigma=S.bar.inv)
    #   Sigma.posterior   = apply(Sigma.posterior,3,solve)
    #   Sigma.posterior   = array(Sigma.posterior,c(N,N,total_S))
    #   A.posterior       = array(rnorm(prod(c(dim(A.bar),total_S))),c(dim(A.bar),total_S))
    #    L                 = t(chol(V.bar))
    
    
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
  
  return(output)
}

```

#######################use thing######## have not tessss
```{r}
posterior.draws.extended = function (total_S, Y, X){
  for (s in 1:total_S){
    # NIW posterior parameters
    #前面一堆 53X53
    V.bar.inv              = t(X)%*%solve(lambda.posterior[s]*I.matrix)%*%X + diag(1/diag(V.prior)) 
    V.bar                  = solve(V.bar.inv)
    A.bar                  = V.bar%*%(t(X)%*%solve(lambda.posterior[s]*I.matrix)%*%Y + diag(1/diag(V.prior))%*%A.prior)
    nu.bar                 = nrow(Y) + nu.prior
    S.bar                  = S.prior + t(Y)%*%solve(lambda.posterior[s]*I.matrix)%*%Y +t(A.prior)%*%diag(1/diag(V.prior))%*%A.prior - t(A.bar)%*%V.bar.inv%*%A.bar
    S.bar.inv              = solve(S.bar)
    
    # posterior draws for A and Sigma
    Sigma.posterior.IW     = rWishart(1, df=nu.bar, Sigma=S.bar.inv)
    Sigma.posterior.draw   = apply(Sigma.posterior.IW,3,solve)
    Sigma.posterior[,,s]   = Sigma.posterior.draw
    A.posterior[,,s]       = array(rnorm(prod(c(dim(A.bar),1))),c(dim(A.bar),1))
    L                      = t(chol(V.bar))
    A.posterior[,,s]       = A.bar + L%*%A.posterior[,,s]%*%chol(Sigma.posterior[,,s])
    
    
    # Update parameters for lambda posterior
    #   a                             = 2 / lambda.priors$alpha
    #   deviation_matrix              = A.posterior[,,s] - A.prior
    #   weighted_deviation_squared    = t(deviation_matrix) %*% solve(V.prior) %*% deviation_matrix
    #   b                             = sum(diag(solve(Sigma.posterior[,,s] %*% weighted_deviation_squared)))
    # p                             = - (T * N) / 2 + 1     
    # Update parameters for lambda posterior
    p                      = -1/2*nrow(Y)*ncol(Y)+1          # N=10
    diff_A                 = Y - X%*%A.posterior[,,s]
    product                = t(diff_A) %*% diff_A
    b                      = sum(diag(solve(Sigma.posterior[,,s]) %*% product))
    a                      = 2 / lambda.priors$alhpa
    
    # Draw next period value for lambda from GIG distribution
    if (s!=total_S){
      lambda.posterior[s+1] = GIGrvg::rgig(n=1, lambda = p, chi = b, psi = a)
    }
  }
  
  output                 = list(A.posterior.exten = A.posterior[,,(S1+1):S2], 
                                Sigma.posterior.exten = Sigma.posterior[,,(S1+1):S2], 
                                lambda.posterior.exten = lambda.posterior[(S1+1):S2,])
  
  return(output)
}

```

```{r}
#| echo: false
#| message: false
#| warning: false
## Test on basic model
N           = ncol(Y_simulation)                          
p           = frequency(Y_simulation)
A.hat       = solve(t(X_simulation)%*%X_simulation)%*%t(X_simulation)%*%Y_simulation
Sigma.hat   = t(Y_simulation-X_simulation%*%A.hat)%*%(Y_simulation-X_simulation%*%A.hat)/nrow(Y_simulation)

# Prior distribution (with Minnesota prior)
kappa.1             = 0.02^2                                     
kappa.2             = 100                                   
A.prior             = matrix(0,nrow(A.hat),ncol(A.hat))
A.prior[2:(N + 1),] = diag(N)
V.prior             = diag(c(kappa.2,kappa.1*((1:p)^(-2))%x%rep(1,N)))
S.prior             = diag(diag(Sigma.hat))
nu.prior            = N+1
#I.matrix            = diag(1,nrow(Y_simulation),nrow(Y_simulation))

# conduct simulation
posterior.extended = posterior.draws.extended(total_S = total_S, Y=Y_simulation, X=X_simulation)
```

```{r}
Sigma_posterior_mean <- apply(posterior.extended$Sigma.posterior, 1:2, mean)

Sigma_df <- as.data.frame(Sigma_posterior_mean)
colnames(Sigma_df) <- c("Simulation_Y1", "Simulation_Y2")
rownames(Sigma_df) <- c("Y1-Lag", "Y2-Lag")
knitr::kable(Sigma_df, caption = "Posterior mean of the covariance matrix Sigma")
```

```{r}
A_posterior_means <- apply(posterior.extended$A.posterior, 1:2, mean)

A_df <- as.data.frame(A_posterior_means)
colnames(A_df) <- c("Simulation_Y1", "Simulation_Y2")
rownames(A_df) <- c("Constant", "Y1-Lag", "Y2-Lag")
knitr::kable(A_df, caption = "Posterior mean of the autoregressive coefficient matrix A")
```
Similarly, the posterior mean of the autoregressive and the covariance matrices are close to an identity matrix and the posterior mean of the constant term is close to zero, so we can conclude that the extended model is also valid.

# Forecasting
The forecast will focus on two models with the sample period from 1990 Q1 to 2023Q4, and forecast how the exchange rate changes for the future up to 2026Q4.

## Forecasting with basic model
```{r forecasting static data}
#| echo: false
#| message: false
#| warning: false
## Present data X, Y
y             = ts(merged_data[,1:ncol(merged_data)])
Y             = ts(y[5:nrow(y),], frequency=4)
X             = matrix(1,nrow(Y),1)
for (i in 1:frequency(Y)){
  X           = cbind(X,y[5:nrow(y)-i,])
}

## Pre-setup 
N             = ncol(Y)
p             = frequency(Y)
A.hat         = solve(t(X)%*%X)%*%t(X)%*%Y
Sigma.hat     = t(Y-X%*%A.hat)%*%(Y-X%*%A.hat)/nrow(Y)

# Prior distribution specification - Minnesota prior 
kappa.1       = 0.02^2                                   
kappa.2       = 100                                   
A.prior       = matrix(0,nrow(A.hat),ncol(A.hat))
A.prior[2,1]  = 1
V.prior       = diag(c(kappa.2,kappa.1*((1:p)^(-2))%x%rep(1,N)))
S.prior       = diag(diag(Sigma.hat))
nu.prior      = N+1

h                      = 12
S                      = 50000
Y.h                    = array(NA,c(h,N,S))

## Applying function 
posterior.sample.draws = posterior.draws(S=50000, Y=Y, X=X)
A.posterior.simu       = posterior.sample.draws$A.posterior
Sigma.posterior.simu   = posterior.sample.draws$Sigma.posterior
```

```{r}
#| echo: false
#| message: false
#| warning: false
# sampling predictive density
for (s in 1:S){
  A.posterior.draw     = A.posterior.simu[,,s]
  Sigma.posterior.draw = Sigma.posterior.simu[,,s]
  x.Ti               = Y[(nrow(Y)-p+1):nrow(Y),]
  x.Ti               = x.Ti[p:1,]
  for (i in 1:h){
    x.T               = c(1,as.vector(t(x.Ti)))
    Y.f               = rmvnorm(1, mean = x.T%*%A.posterior.draw, sigma=Sigma.posterior.draw)
    x.Ti            = rbind(Y.f,x.Ti[1:(p-1),])
    Y.h[i,,s]         = Y.f[1:N]
  }
}
```


```{r}
ex_rate.point.f    = apply(Y.h[,1,],1,mean) 
ex_rate.interval.f = apply(Y.h[,1,],1,hdi,credMass=0.15)
ex_rate.range      = range(y[,1],ex_rate.interval.f)

true_y_value <- ex_rate.point.f[12]  # Set this to your actual true y-value

# Define the range and labels for the y-axis
y_min <- min(ex_rate.range[1], true_y_value)  # Adjust as necessary to include true_y_value
y_max <- max(ex_rate.range[2], true_y_value)
num_labels <- 4  # You can adjust the number of labels

# Generate a sequence of y-values for the axis that includes true_y_value
y_values <- pretty(c(y_min, y_max), num_labels)
if(!true_y_value %in% y_values) {
  y_values <- sort(c(y_values))
}

# Generate labels for the y-axis, ensuring true_y_value is included and highlighted
y_labels <- sprintf("%.2f", y_values)
y_labels[y_values == true_y_value] <- paste(y_labels[y_values == true_y_value], "(True Value)", sep=" ")

# Plot adjustments
plot(1:(length(y[,1])+h), c(y[,1], ex_rate.point.f), type="l", ylim=c(min(y_values), max(y_values)), axes=FALSE, xlab="", ylab="", lwd=2, col=mcxs1)
axis(1, c(1, 21, 41, 61, 81, 101, nrow(y), nrow(y)+h), c("1992", "1997", "2002", "2007", "2012", "2017", "", ""), col="black")
axis(2, at=y_values, labels=y_labels, col="black")

# Red dashed line for the true y-value
abline(h=true_y_value, col="red", lty=2, lwd=1)


text(x=nrow(y), y=9.65, srt=90, "2025-12")
abline(v=nrow(y), col=mcxs4)
text(x=nrow(y)+h, y=9.65, srt=90, "2026-12")
abline(v=nrow(y)+h, col=mcxs2)
legend(136, 1.2, legend=c("Y2023Q4", "Y2026Q4"), col=c(mcxs4, mcxs2), lty=1, cex=0.45)

polygon(c(length(y[,1]):(length(y[,1])+h),(length(y[,1]):(length(y[,1])+h))[13:1]),
        c(y[136,1],ex_rate.interval.f[1,],ex_rate.interval.f[2,12:1],y[136,1]),
        col=mcxs1.shade1, border=mcxs1.shade1)

```

```{r}
### Forecasting on extended model
S1                = 5000                            # will be discard
S2                = 45000                            
total_S           = S1+S2
A.posterior       = array(NA, dim = c((1+N*p),N,S1+S2))
Sigma.posterior   = array(NA, dim = c(N,N,S1+S2))
lambda.posterior  = matrix(NA, S1+S2, 1)
#I.matrix            = diag(1,nrow(Y),nrow(Y))

# set the initial value of lambda
lambda.posterior[1] = 5                             

# parameter alpha
lambda.priors = list(alpha = 1.5)

posterior.ext              = posterior.draws.extended(total_S = total_S, Y=Y, X=X)
A.posterior.ext.simu       = posterior.ext$A.posterior.exten[,,(S1+1):total_S]
Sigma.posterior.ext.simu   = posterior.ext$Sigma.posterior.exten[,,(S1+1):total_S]

#posterior.extend.draws     = posterior.draws.extended(total_S = total_S, Y=Y, X=X)
#A.posterior.ext.simu       = posterior.extend.draws[["A.posterior.exten"]]
#Sigma.posterior.ext.simu   = posterior.extend.draws[["Sigma.posterior.exten"]]

## Three-year ahead forecasting h=12
# set up
h                          = 12
S                          = 45000
#S                          = 40000
Y.h.ext                    = array(NA,c(h,N,S))
```

```{r}
# sampling predictive density
for (s in 1:S){
  A.posterior.draw         = A.posterior.ext.simu[,,s]
  Sigma.posterior.draw     = Sigma.posterior.ext.simu[,,s]
  x.Ti                   = Y[(nrow(Y)-p+1):nrow(Y),]
  x.Ti                   = x.Ti[p:1,]
  for (i in 1:h){
    x.T                    = c(1,as.vector(t(x.Ti)))
    Y.f                    = rmvnorm(1, mean = x.T%*%A.posterior.draw, sigma=Sigma.posterior.draw)
    x.Ti                 = rbind(Y.f,x.Ti[1:(p-1),])
    Y.h.ext[i,,s]          = Y.f[1:N]
  }
}
```


#### 90%, 68% and 15% highest density intervals respectively
```{r}
par(mfrow = c(3, 2), mar = c(2, 3, 1, 1), oma = c(0, 0, 4, 0))

ex_rate.point.f    = apply(Y.h[,1,],1,mean) 
ex_rate.interval.f = apply(Y.h[,1,],1,hdi,credMass=0.90)
ex_rate.range      = range(y[,1],ex_rate.interval.f)

true_y_value <- ex_rate.point.f[12]  # Set this to your actual true y-value

# Define the range and labels for the y-axis
y_min <- min(ex_rate.range[1], true_y_value)  # Adjust as necessary to include true_y_value
y_max <- max(ex_rate.range[2], true_y_value)
num_labels <- 4  # You can adjust the number of labels

# Generate a sequence of y-values for the axis that includes true_y_value
y_values <- pretty(c(y_min, y_max), num_labels)
if(!true_y_value %in% y_values) {
  y_values <- sort(c(y_values))
}

# Generate labels for the y-axis, ensuring true_y_value is included and highlighted
y_labels <- sprintf("%.2f", y_values)
y_labels[y_values == true_y_value] <- paste(y_labels[y_values == true_y_value], "(True Value)", sep=" ")

# Plot adjustments
plot(1:(length(y[,1])+h), c(y[,1], ex_rate.point.f), type="l", ylim=c(min(y_values), max(y_values)), axes=FALSE, xlab="", ylab="", lwd=2, col=mcxs1)
axis(1, c(1, 21, 41, 61, 81, 101, nrow(y), nrow(y)+h), c("1992", "1997", "2002", "2007", "2012", "2017", "", ""), col="black")
axis(2, at=y_values, labels=y_labels, col="black",las=1)

# Red dashed line for the true y-value
abline(h=true_y_value, col="red", lty=2, lwd=1)


text(x=nrow(y), y=9.65, srt=90, "2025-12")
abline(v=nrow(y), col=mcxs4)
text(x=nrow(y)+h, y=9.65, srt=90, "2026-12")
abline(v=nrow(y)+h, col=mcxs2)
legend(136, 1.2, legend=c("Y2023Q4", "Y2026Q4"), col=c(mcxs4, mcxs2), lty=1, cex=0.35)

polygon(c(length(y[,1]):(length(y[,1])+h),(length(y[,1]):(length(y[,1])+h))[13:1]),
        c(y[136,1],ex_rate.interval.f[1,],ex_rate.interval.f[2,12:1],y[136,1]),
        col=mcxs1.shade1, border=mcxs1.shade1)


ex_rate.point.f    = apply(Y.h.ext[,1,],1,mean) 
ex_rate.interval.f = apply(Y.h.ext[,1,],1,hdi,credMass=0.90)
ex_rate.range      = range(y[,1],ex_rate.interval.f)         


true_y_value <- ex_rate.point.f[12]  # Set this to your actual true y-value

# Define the range and labels for the y-axis
y_min <- min(ex_rate.range[1], true_y_value)  # Adjust as necessary to include true_y_value
y_max <- max(ex_rate.range[2], true_y_value)
num_labels <- 4  # You can adjust the number of labels

# Generate a sequence of y-values for the axis that includes true_y_value
y_values <- pretty(c(y_min, y_max), num_labels)
if(!true_y_value %in% y_values) {
  y_values <- sort(c(y_values))
}

# Generate labels for the y-axis, ensuring true_y_value is included and highlighted
y_labels <- sprintf("%.2f", y_values)
y_labels[y_values == true_y_value] <- paste(y_labels[y_values == true_y_value], "(True Value)", sep=" ")

# Plot adjustments
plot(1:(length(y[,1])+h), c(y[,1], ex_rate.point.f), type="l", ylim=c(min(y_values), max(y_values)), xlab="", ylab="", lwd=2, col=mcxs1, axes=FALSE)
axis(1, c(1, 21, 41, 61, 81, 101, nrow(y), nrow(y)+h), c("1992", "1997", "2002", "2007", "2012", "2017", "", ""), col="black")
axis(2, at=y_values, labels=y_labels, col="black",las=1)

# Red dashed line for the true y-value
abline(h=true_y_value, col="red", lty=2, lwd=1)

text(x=nrow(y), y=9.65, srt=90, "")
abline(v=nrow(y), col=mcxs4)
text(x=nrow(y)+h, y=9.65, srt=90, "")
abline(v=nrow(y)+h, col=mcxs2)
legend(136, 1.2, legend=c("Y2023Q4", "Y2026Q4"), col=c(mcxs4, mcxs2), lty=1, cex=0.35)

polygon(c(length(y[,1]):(length(y[,1])+h),(length(y[,1]):(length(y[,1])+h))[13:1]),
        c(y[136,1],ex_rate.interval.f[1,],ex_rate.interval.f[2,12:1],y[136,1]),
        col=mcxs1.shade1, border=mcxs1.shade1)

#### 68% highest density intervals

#par(mfrow = c(1, 2), mar = c(4, 4, 1, 1), oma = c(0, 0, 6, 0))

ex_rate.point.f    = apply(Y.h[,1,],1,mean) 
ex_rate.interval.f = apply(Y.h[,1,],1,hdi,credMass=0.68)
ex_rate.range      = range(y[,1],ex_rate.interval.f)

true_y_value <- ex_rate.point.f[12]  # Set this to your actual true y-value

# Define the range and labels for the y-axis
y_min <- min(ex_rate.range[1], true_y_value)  # Adjust as necessary to include true_y_value
y_max <- max(ex_rate.range[2], true_y_value)
num_labels <- 4  # You can adjust the number of labels

# Generate a sequence of y-values for the axis that includes true_y_value
y_values <- pretty(c(y_min, y_max), num_labels)
if(!true_y_value %in% y_values) {
  y_values <- sort(c(y_values))
}

# Generate labels for the y-axis, ensuring true_y_value is included and highlighted
y_labels <- sprintf("%.2f", y_values)
y_labels[y_values == true_y_value] <- paste(y_labels[y_values == true_y_value], "(True Value)", sep=" ")

# Plot adjustments
plot(1:(length(y[,1])+h), c(y[,1], ex_rate.point.f), type="l", ylim=c(min(y_values), max(y_values)), axes=FALSE, xlab="", ylab="", lwd=2, col=mcxs1)
axis(1, c(1, 21, 41, 61, 81, 101, nrow(y), nrow(y)+h), c("1992", "1997", "2002", "2007", "2012", "2017", "", ""), col="black")
axis(2, at=y_values, labels=y_labels, col="black",las=1)

# Red dashed line for the true y-value
abline(h=true_y_value, col="red", lty=2, lwd=1)


text(x=nrow(y), y=9.65, srt=90, "2025-12")
abline(v=nrow(y), col=mcxs4)
text(x=nrow(y)+h, y=9.65, srt=90, "2026-12")
abline(v=nrow(y)+h, col=mcxs2)
#legend(136, 1.2, legend=c("Y2023Q4", "Y2026Q4"), col=c(mcxs4, mcxs2), lty=1, cex=0.35)

polygon(c(length(y[,1]):(length(y[,1])+h),(length(y[,1]):(length(y[,1])+h))[13:1]),
        c(y[136,1],ex_rate.interval.f[1,],ex_rate.interval.f[2,12:1],y[136,1]),
        col=mcxs1.shade1, border=mcxs1.shade1)


ex_rate.point.f    = apply(Y.h.ext[,1,],1,mean) 
ex_rate.interval.f = apply(Y.h.ext[,1,],1,hdi,credMass=0.68)
ex_rate.range      = range(y[,1],ex_rate.interval.f)         


true_y_value <- ex_rate.point.f[12]  # Set this to your actual true y-value

# Define the range and labels for the y-axis
y_min <- min(ex_rate.range[1], true_y_value)  # Adjust as necessary to include true_y_value
y_max <- max(ex_rate.range[2], true_y_value)
num_labels <- 4  # You can adjust the number of labels

# Generate a sequence of y-values for the axis that includes true_y_value
y_values <- pretty(c(y_min, y_max), num_labels)
if(!true_y_value %in% y_values) {
  y_values <- sort(c(y_values))
}

# Generate labels for the y-axis, ensuring true_y_value is included and highlighted
y_labels <- sprintf("%.2f", y_values)
y_labels[y_values == true_y_value] <- paste(y_labels[y_values == true_y_value], "(True Value)", sep=" ")

# Plot adjustments
plot(1:(length(y[,1])+h), c(y[,1], ex_rate.point.f), type="l", ylim=c(min(y_values), max(y_values)), xlab="", ylab="", lwd=2, col=mcxs1, axes=FALSE)
axis(1, c(1, 21, 41, 61, 81, 101, nrow(y), nrow(y)+h), c("1992", "1997", "2002", "2007", "2012", "2017", "", ""), col="black")
axis(2, at=y_values, labels=y_labels, col="black",las=1)

# Red dashed line for the true y-value
abline(h=true_y_value, col="red", lty=2, lwd=1)

text(x=nrow(y), y=9.65, srt=90, "")
abline(v=nrow(y), col=mcxs4)
text(x=nrow(y)+h, y=9.65, srt=90, "")
abline(v=nrow(y)+h, col=mcxs2)
#legend(136, 1.2, legend=c("Y2023Q4", "Y2026Q4"), col=c(mcxs4, mcxs2), lty=1, cex=0.35)

polygon(c(length(y[,1]):(length(y[,1])+h),(length(y[,1]):(length(y[,1])+h))[13:1]),
        c(y[136,1],ex_rate.interval.f[1,],ex_rate.interval.f[2,12:1],y[136,1]),
        col=mcxs1.shade1, border=mcxs1.shade1)


#### 15% highest density intervals

ex_rate.point.f    = apply(Y.h[,1,],1,mean) 
ex_rate.interval.f = apply(Y.h[,1,],1,hdi,credMass=0.15)
ex_rate.range      = range(y[,1],ex_rate.interval.f)

true_y_value <- ex_rate.point.f[12]  # Set this to your actual true y-value

# Define the range and labels for the y-axis
y_min <- min(ex_rate.range[1], true_y_value)  # Adjust as necessary to include true_y_value
y_max <- max(ex_rate.range[2], true_y_value)
num_labels <- 4  # You can adjust the number of labels

# Generate a sequence of y-values for the axis that includes true_y_value
y_values <- pretty(c(y_min, y_max), num_labels)
if(!true_y_value %in% y_values) {
  y_values <- sort(c(y_values))
}

# Generate labels for the y-axis, ensuring true_y_value is included and highlighted
y_labels <- sprintf("%.2f", y_values)
y_labels[y_values == true_y_value] <- paste(y_labels[y_values == true_y_value], "(True Value)", sep=" ")

# Plot adjustments
plot(1:(length(y[,1])+h), c(y[,1], ex_rate.point.f), type="l", ylim=c(min(y_values), max(y_values)), axes=FALSE, xlab="", ylab="", lwd=2, col=mcxs1)
axis(1, c(1, 21, 41, 61, 81, 101, nrow(y), nrow(y)+h), c("1992", "1997", "2002", "2007", "2012", "2017", "", ""), col="black")
axis(2, at=y_values, labels=y_labels, col="black",las=1)

# Red dashed line for the true y-value
abline(h=true_y_value, col="red", lty=2, lwd=1)


text(x=nrow(y), y=9.65, srt=90, "2025-12")
abline(v=nrow(y), col=mcxs4)
text(x=nrow(y)+h, y=9.65, srt=90, "2026-12")
abline(v=nrow(y)+h, col=mcxs2)
#legend(136, 1.2, legend=c("Y2023Q4", "Y2026Q4"), col=c(mcxs4, mcxs2), lty=1, cex=0.35)

polygon(c(length(y[,1]):(length(y[,1])+h),(length(y[,1]):(length(y[,1])+h))[13:1]),
        c(y[136,1],ex_rate.interval.f[1,],ex_rate.interval.f[2,12:1],y[136,1]),
        col=mcxs1.shade1, border=mcxs1.shade1)


ex_rate.point.f    = apply(Y.h.ext[,1,],1,mean) 
ex_rate.interval.f = apply(Y.h.ext[,1,],1,hdi,credMass=0.15)
ex_rate.range      = range(y[,1],ex_rate.interval.f)         


true_y_value <- ex_rate.point.f[12]  # Set this to your actual true y-value

# Define the range and labels for the y-axis
y_min <- min(ex_rate.range[1], true_y_value)  # Adjust as necessary to include true_y_value
y_max <- max(ex_rate.range[2], true_y_value)
num_labels <- 4  # You can adjust the number of labels

# Generate a sequence of y-values for the axis that includes true_y_value
y_values <- pretty(c(y_min, y_max), num_labels)
if(!true_y_value %in% y_values) {
  y_values <- sort(c(y_values))
}

# Generate labels for the y-axis, ensuring true_y_value is included and highlighted
y_labels <- sprintf("%.2f", y_values)
y_labels[y_values == true_y_value] <- paste(y_labels[y_values == true_y_value], "(True Value)", sep=" ")

# Plot adjustments
plot(1:(length(y[,1])+h), c(y[,1], ex_rate.point.f), type="l", ylim=c(min(y_values), max(y_values)), xlab="", ylab="", lwd=2, col=mcxs1, axes=FALSE)
axis(1, c(1, 21, 41, 61, 81, 101, nrow(y), nrow(y)+h), c("1992", "1997", "2002", "2007", "2012", "2017", "", ""), col="black")
axis(2, at=y_values, labels=y_labels, col="black",las=1)

# Red dashed line for the true y-value
abline(h=true_y_value, col="red", lty=2, lwd=1)

text(x=nrow(y), y=9.65, srt=90, "")
abline(v=nrow(y), col=mcxs4)
text(x=nrow(y)+h, y=9.65, srt=90, "")
abline(v=nrow(y)+h, col=mcxs2)
#legend(136, 1.2, legend=c("Y2023Q4", "Y2026Q4"), col=c(mcxs4, mcxs2), lty=1, cex=0.35)

polygon(c(length(y[,1]):(length(y[,1])+h),(length(y[,1]):(length(y[,1])+h))[13:1]),
        c(y[136,1],ex_rate.interval.f[1,],ex_rate.interval.f[2,12:1],y[136,1]),
        col=mcxs1.shade1, border=mcxs1.shade1)
```
The above presents the historical and forecasting data the AUD/USD exchange rate for baisc and extended model. 

For the point forecast, for basic model, the exchange rate shows several peaks and troughs, indicating the volatility in the exchange rate over the years. For the three years forecasting, it shows a clear downward trend and reaching the lowest above 0.6 in 2026. And for extended model, it also shows a clear downward trend for the first half forecasting period and reaching below 0.06 in around 2025 and slightly increase back to 0.61 fo the following period. 

Regard to the 3D with density intervals below for both model forecasting, we could notice that the for each different predictive density at specific horizons, with the back wall we have the one period ahead predictive density and with the front we have the 12 period ahead density which is 3 years. We could see the distribution becomes lower and more dispersed with the increases of the horizon, as the data is more informative about the nearest developments in the future. Hence, one period predictive density is highly concentrated relative to others with smaller variance and taller peak. Similarly, the interval become more wider and dispersed resulting a more uncertainty for the future period forecasting.

```{r}
#| echo: false
#| message: false
#| warning: false
par(mfrow=c(1,2), mar=c(2,2,1,1))
limits.1    = range(Y.h[,1,])
point.f     = apply(Y.h[,1,],1,mean)
interval.f  = apply(Y.h[,1,],1,hdi,credMass=0.90)

x           = seq(from=limits.1[1], to=limits.1[2], length.out=100)
z           = matrix(NA,h,99)
for (i in 1:h){
  z[i,]     = hist(Y.h[i,1,], breaks=x, plot=FALSE)$density
}
x           = hist(Y.h[i,1,], breaks=x, plot=FALSE)$mids
yy          = 1:h
z           = t(z)

theta = 180
phi   = 15.5
f4    = persp3D(x=x, y=yy, z=z, phi=phi, theta=theta, xlab="\nerate[t+h|t]", ylab="h", zlab="\npredictive densities of erate", shade=NA, border=NA, ticktype="detailed", nticks=3,cex.lab=1, col=NA,plot=FALSE)
perspbox (x=x, y=yy, z=z, bty="f", col.axis="black", phi=phi, theta=theta, xlab="\nerate[t+h|t]", ylab="h", zlab="\npredictive densities of erate", ticktype="detailed", nticks=3,cex.lab=1, col = NULL, plot = TRUE)
polygon3D(x=c(interval.f[1,],interval.f[2,h:1]), y=c(1:h,h:1), z=rep(0,2*h), col = mcxs1.shade1, NAcol = "white", border = NA, add = TRUE, plot = TRUE)
for (i in 1:h){
  f4.l = trans3d(x=x, y=yy[i], z=z[,i], pmat=f4)
  lines(f4.l, lwd=0.5, col="black")
}
f4.l1 = trans3d(x=point.f, y=yy, z=0, pmat=f4)
lines(f4.l1, lwd=2, col=mcxs1)


limits.1    = range(Y.h.ext[,1,])
point.f     = apply(Y.h.ext[,1,],1,mean)
interval.f  = apply(Y.h.ext[,1,],1,hdi,credMass=0.90)

x           = seq(from=limits.1[1], to=limits.1[2], length.out=100)
z           = matrix(NA,h,99)
for (i in 1:h){
  z[i,]     = hist(Y.h.ext[i,1,], breaks=x, plot=FALSE)$density
}
x           = hist(Y.h.ext[i,1,], breaks=x, plot=FALSE)$mids
yy          = 1:h
z           = t(z)

theta = 180
phi   = 15.5
f4    = persp3D(x=x, y=yy, z=z, phi=phi, theta=theta, xlab="\nerate[t+h|t]", ylab="h", zlab="\npredictive densities of erate", shade=NA, border=NA, ticktype="detailed", nticks=3,cex.lab=1, col=NA,plot=FALSE)
perspbox (x=x, y=yy, z=z, bty="f", col.axis="black", phi=phi, theta=theta, xlab="\nerate[t+h|t]", ylab="h", zlab="\npredictive densities of erate", ticktype="detailed", nticks=3,cex.lab=1, col = NULL, plot = TRUE)
polygon3D(x=c(interval.f[1,],interval.f[2,h:1]), y=c(1:h,h:1), z=rep(0,2*h), col = mcxs1.shade1, NAcol = "white", border = NA, add = TRUE, plot = TRUE)
for (i in 1:h){
  f4.l = trans3d(x=x, y=yy[i], z=z[,i], pmat=f4)
  lines(f4.l, lwd=0.5, col="black")
}
f4.l1 = trans3d(x=point.f, y=yy, z=0, pmat=f4)
lines(f4.l1, lwd=2, col=mcxs2)
```
Interactive versions of the above 3D plots are provided below.
```{r}
#| echo: false
#| message: false
#| warning: false
par(mfrow=c(1,2), mar=c(2,2,1,1))

# Forecasting on Extended model
limits.1        = range(Y.h[,1,])
point.f         = apply(Y.h[,1,],1,mean)
interval.f      = apply(Y.h[,1,],1,hdi,credMass=0.90)
theta = 180
phi   = 15.5

x.erate           = seq(from=limits.1[1], to=limits.1[2], length.out=10)
z.erate           = matrix(NA,h,9)
for (i in 1:h){
  z.erate[i,]     = hist(Y.h[i,1,], breaks=x.erate, plot=FALSE)$density
}
x.erate           = hist(Y.h[i,1,], breaks=x.erate, plot=FALSE)$mids
yy.erate          = 1:h

# plot using plot_ly
par(mfrow=c(1,1))
plot_ly(y = yy.erate, x = x.erate, z=z.erate) %>%
  
  layout(scene=list(xaxis=list(title="AUD/USD"),
                    yaxis=list(title="h-step forecast"),
                    zaxis=list(title="density")),
         title = "AUD/USD forecast densities for basic model") %>%
  add_surface(colors = c(mcxs1,mcxs4,mcxs5))


# Forecasting on Extended model
limits.1        = range(Y.h.ext[,1,])
point.f         = apply(Y.h.ext[,1,],1,mean)
interval.f      = apply(Y.h.ext[,1,],1,hdi,credMass=0.90)
theta = 180
phi   = 15.5

x.erate           = seq(from=limits.1[1], to=limits.1[2], length.out=10)
z.erate           = matrix(NA,h,9)
for (i in 1:h){
  z.erate[i,]     = hist(Y.h.ext[i,1,], breaks=x.erate, plot=FALSE)$density
}
x.erate           = hist(Y.h.ext[i,1,], breaks=x.erate, plot=FALSE)$mids
yy.erate          = 1:h

# plot using plot_ly
par(mfrow=c(1,1))
plot_ly(y = yy.erate, x = x.erate, z=z.erate) %>%
  
  layout(scene=list(xaxis=list(title="AUD/USD"),
                    yaxis=list(title="h-step forecast"),
                    zaxis=list(title="density")),
         title = "AUD/USD forecast densities for extended model") %>%
  add_surface(colors = c(mcxs1,"magenta2",mcxs5))

#[which indicates the extended model can be more informative in forecasting as it has a higher certainty from a more concentrated distribution.]
```
The 3D plots above allow us to access to alternative vantage points. We could notice that for the first few forecasting periods, the extended model has very sharp and concentrated densities around the point estimated, and it shows clearly heavy tails, which may increase the possibility of capturing extreme values that deviate significantly from the central forecast.
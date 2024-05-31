library(Rmpfr)
library(MASS)
library(dplyr)
library(psych)

library(fredr)
library(readrba)
library(readabs)
library(xts)
library(tseries)
library(fUnitRoots) 
library(plot3D)
library(MASS)
library(HDInterval)
library(plotly)       
library(mgcv)
library(mvtnorm)
library(Rmpfr)
library(dplyr)
library(psych)
fredr_set_key("75b470c4883ecfd5a7b4185f30437bd0")

# Define colors
mcxs1  = "#05386B"
mcxs2  = "#379683"
mcxs3  = "#5CDB95"
mcxs4  = "#8EE4AF"
mcxs5  = "#EDF5E1"

mcxs1.rgb   = col2rgb(mcxs1)
mcxs1.shade1= rgb(mcxs1.rgb[1],mcxs1.rgb[2],mcxs1.rgb[3], alpha=120, maxColorValue=255)
mcxs2.rgb   = col2rgb(mcxs2)
mcxs2.shade1= rgb(mcxs2.rgb[1],mcxs2.rgb[2],mcxs2.rgb[3], alpha=120, maxColorValue=255)


#Y variable
# AUD/USD exchange rate quarterly
ex_rate <- read_rba(series_id = "FXRUSD")
ex_rate$date <- as.Date(ex_rate$date)
erate <- xts(ex_rate$value, ex_rate$date)
erate <- apply.quarterly(erate,mean)
index(erate) <- seq(as.Date("1969-09-01"), by = "3 months", length.out = nrow(erate))


# Australia real gdp seasonal adjusted quarterly
#rgdp_au <- read_abs(series_id = "A2304404C")  
rgdp_au <- read_rba(series_id = "GGDPCVGDP")
rgdp_au <- xts::xts(rgdp_au$value, rgdp_au$date)
index(rgdp_au)   <- seq(as.Date("1959-09-01"), by = "3 months", length.out = nrow(rgdp_au))

#cash rate/interest rate of AUS quartly
cashrate<- read_cashrate(type = c("target"))
crate_au<- xts(cashrate$value, cashrate$date)
crate_au<- apply.quarterly(crate_au,mean)
crate_au<- xts(crate_au, seq(as.Date("1990-03-01"), by = "quarter", length.out = length(crate_au)))

#CPI quartly
# cpi_au <- read_rba(series_id = "GCPIAG")
cpi_au <- read_abs(series_id = "A2325846C")  
cpi_au <- xts::xts(cpi_au$value, cpi_au$date)

#unemployment rate quartly
#unemprate <-read_rba(series_id = "GLFSURSA")
unemprate <- read_abs(series_id = "A84423050A") 
unemr_au<- xts(unemprate$value, unemprate$date)
unemr_au<- apply.quarterly(unemr_au,mean)

# International Trade in Goods and Services seasonal adjusted_quartly
exportaus <- read_abs(series_id = "A2718603V")   
expor_au<- xts(exportaus $value, exportaus $date)
expor_au<- abs(expor_au)
expor_au<- apply.quarterly(expor_au,mean)

importaus <- read_abs(series_id = "A2718577A")     
impor_au<- xts(importaus$value, importaus$date)
impor_au<- apply.quarterly(impor_au,mean)

# America data
# us gdp
#rgdpus <- fredr(series_id = "A939RX0Q048SBEA")
rgdpus     <- fredr(series_id = "GDPC1")
rgdp_us     <- to.quarterly(xts(rgdpus$value, rgdpus$date), OHLC = FALSE)
index(rgdp_us) <- seq(as.Date("1947-03-01"), by = "3 months", length.out = nrow(rgdp_us))

#Federal Funds Effective Rate/interest rate quartly
usdratelink = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DFF&scale=left&cosd=1954-07-01&coed=2024-03-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily%2C%207-Day&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-04-01&revision_date=2024-04-01&nd=1954-07-01"
crate_us <- read.csv(usdratelink)
crate_us$DATE <- as.Date(crate_us$DATE)
crate_us <- xts(crate_us$DFF, order.by = crate_us$DATE)
crate_us <- apply.quarterly(crate_us,mean)
crate_us<- xts(crate_us, seq(as.Date("1954-09-01"), by = "quarter", length.out = length(crate_us)))

# cpi quartly
cpiusd  <- fredr(series_id = "USACPIALLMINMEI")
cpi_us<- xts(cpiusd$value, cpiusd$date)
cpi_us<- apply.quarterly(cpi_us,mean)

# unemployment quartly
unemprate_usd = fredr(series_id = "UNRATE")
unemr_us <- xts(unemprate_usd$value, unemprate_usd$date)
unemr_us<- apply.quarterly(unemr_us,mean)

#export_usd——quartly
usdexportink = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=EXPGS&scale=left&cosd=1947-01-01&coed=2023-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-03-30&revision_date=2024-03-30&nd=1947-01-01"
expor_us <- read.csv(usdexportink)
expor_us$DATE <- as.Date(expor_us$DATE)
expor_us <- xts::xts(expor_us$EXPGS, order.by = expor_us$DATE)
index(expor_us) <- seq(as.Date("1947-03-01"), by = "3 months", length.out = nrow(expor_us))

#import_usd_quartly
usdimportlink = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=IMPGS&scale=left&cosd=1947-01-01&coed=2023-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-03-30&revision_date=2024-03-30&nd=1947-01-01"
impor_us <- read.csv(usdimportlink)
impor_us$DATE <- as.Date(impor_us$DATE)
impor_us <- xts::xts(impor_us$IMPGS, order.by = impor_us$DATE)
index(impor_us) <- seq(as.Date("1947-03-01"), by = "3 months", length.out = nrow(impor_us))


#| echo: false
#| message: false
#| warning: false

# log transformation of data
variables <- c("cpi_au", "cpi_us", "rgdp_au", "rgdp_us", "impor_au", "impor_us", "expor_au", "expor_us")

for(var in variables) {
  assign(var, log(get(var)))
}


#| echo: false
#| message: false
#| warning: false

# All Variables
merged_data = na.omit(merge(erate, 
                            cpi_au, cpi_us, 
                            crate_au, crate_us, 
                            expor_au, expor_us,  
                            impor_au, impor_us, 
                            rgdp_au, rgdp_us,
                            unemr_au, unemr_us))

# Defining your column name vector:
variable_names <- c("exchange rate", "cpi_au", "cpi_us", 
                    "cashrate_au", "cashrate_us", "export_au", "export_us",
                    "import_au", "import_us", "realgdp_au", "realgdp_us",
                    "unemployemtrate_au", "unemployemtrate_us")


colnames(merged_data)   <- variable_names

S1= 100
S2= 900
total_S = S1+S2
S=total_S

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

posterior.draws.extended <- function(total_S,Y, X){
  
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
  
  output                 = (list(A.posterior.exten = A.posterior.draws[,,S1:total_S], 
                                 Sigma.posterior.exten = Sigma.posterior.draws[,,S1:total_S], 
                                 lambda.posterior.exten = lambda.posterior.draws[,(S1+1):(S+1)]))
  
}

# conduct simulation
posterior.extended = posterior.draws.extended(total_S = total_S, Y=Y_simulation, X=X_simulation)




Sigma_posterior_mean <- apply(posterior.extended$Sigma.posterior.exten, 1:2, mean)

Sigma_df <- as.data.frame(Sigma_posterior_mean)
colnames(Sigma_df) <- c("Simulation_Y1", "Simulation_Y2")
rownames(Sigma_df) <- c("Y1-Lag", "Y2-Lag")
knitr::kable(Sigma_df, caption = "Posterior mean of the covariance matrix Sigma")

A_posterior_means <- apply(posterior.extended$A.posterior.exten, 1:2, mean)

A_df <- as.data.frame(A_posterior_means)
colnames(A_df) <- c("Simulation_Y1", "Simulation_Y2")
rownames(A_df) <- c("Constant", "Y1-Lag", "Y2-Lag")
knitr::kable(A_df, caption = "Posterior mean of the autoregressive coefficient matrix A")


##########################################Forecasting##############################

S1                = 100                            # will be discard
S2                = 900                            
total_S           = S1+S2
S=total_S

y             = ts(merged_data[,1:ncol(merged_data)])
Y             = ts(y[5:nrow(y),], frequency=4)

X             = matrix(1,nrow(Y),1)
for (i in 1:frequency(Y)){
  X           = cbind(X,y[5:nrow(y)-i,])
}

p             = frequency(Y)
posterior.draws.extended <- function(total_S,Y, X){
  
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
  
 # Omega.inv.test = diag(1/lambda.0)
  
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
  
  output                 = (list(A.posterior.exten = A.posterior.draws[,,S1:total_S], 
                                 Sigma.posterior.exten = Sigma.posterior.draws[,,S1:total_S], 
                                 lambda.posterior.exten = lambda.posterior.draws[,(S1+1):(S+1)]))
  }


posterior.ext              = posterior.draws.extended(total_S = total_S, Y=Y, X=X)
A.posterior.ext.simu       = posterior.ext$A.posterior.exten
Sigma.posterior.ext.simu   = posterior.ext$Sigma.posterior.exten

#posterior.extend.draws     = posterior.draws.extended(total_S = total_S, Y=Y, X=X)
#A.posterior.ext.simu       = posterior.extend.draws[["A.posterior.exten"]]
#Sigma.posterior.ext.simu   = posterior.extend.draws[["Sigma.posterior.exten"]]



## Three-year ahead forecasting h=12
# set up
h                          = 12
S                          = 1000
Y.h.ext                    = array(NA,c(h,N,S))


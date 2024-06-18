library(fredr)
library(readrba)
library(readabs)
library(xts)
library(tseries)
library(fUnitRoots) 
library(MASS)
library(mgcv)
library(lubridate) 
library(mvtnorm)
library(plot3D)
library(plotly)
library(MASS)
library(HDInterval)
fredr_set_key("75b470c4883ecfd5a7b4185f30437bd0")
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
erate <- xts(ex_rate$value, ex_rate$date)
end_quartly <- endpoints(erate, on = "quarters")
erate <- erate[end_quartly]
index(erate) <- seq(as.Date("1969-09-01"), by = "3 months", length.out = nrow(erate))

# Australia real gdp seasonal adjusted quarterly
#rgdp_au <- read_abs(series_id = "A2304404C")  
rgdp_au <- read_rba(series_id = "GGDPCVGDP")
rgdp_au <- xts::xts(rgdp_au$value, rgdp_au$date)
index(rgdp_au)   <- seq(as.Date("1959-09-01"), by = "3 months", length.out = nrow(rgdp_au))

#cash rate/interest rate of AUS quartly
cashrate<- read_cashrate(type = c("target"))
crate_au<- xts(cashrate$value, cashrate$date)
end_quartly <- endpoints(crate_au, on = "quarters")
crate_au <- crate_au[end_quartly]
index(crate_au) <- seq(as.Date("1990-03-01"), by = "quarter", length.out = length(crate_au))

#CPI quartly
# cpi_au <- read_rba(series_id = "GCPIAG")
cpi_au <- read_abs(series_id = "A2325846C")  
cpi_au <- xts::xts(cpi_au$value, cpi_au$date)

#unemployment rate quartly
#unemprate <-read_rba(series_id = "GLFSURSA")
unemprate <- read_abs(series_id = "A84423050A") 
unemr_au<- xts(unemprate$value, unemprate$date)
end_quartly <- endpoints(unemr_au, on = "quarters")
unemr_au <- unemr_au[end_quartly]

# International Trade in Goods and Services seasonal adjusted_quartly
exportaus <- read_abs(series_id = "A2718603V")   
expor_au<- xts(exportaus $value, exportaus$date)
expor_au<- abs(expor_au)
end_quartly <- endpoints(expor_au, on = "quarters")
expor_au <- expor_au[end_quartly]

importaus <- read_abs(series_id = "A2718577A")     
impor_au<- xts(importaus$value, importaus$date)
end_quartly <- endpoints(impor_au, on = "quarters")
impor_au <- impor_au[end_quartly]

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
end_quartly <- endpoints(crate_us, on = "quarters")
crate_us <- crate_us[end_quartly]
index(crate_us) <- seq(as.Date("1954-09-01"), by = "quarter", length.out = length(crate_us))

# cpi quartly
cpiusd  <- fredr(series_id = "USACPIALLMINMEI")
cpi_us<- xts(cpiusd$value, cpiusd$date)
end_quartly <- endpoints(cpi_us, on = "quarters")
cpi_us <- cpi_us[end_quartly]

# unemployment quartly
unemprate_usd = fredr(series_id = "UNRATE")
unemr_us <- xts(unemprate_usd$value, unemprate_usd$date)
end_quartly <- endpoints(unemr_us, on = "quarters")
unemr_us <- unemr_us[end_quartly]

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


# log transformation of data
variables <- c("cpi_au", "cpi_us", "rgdp_au", "rgdp_us", "impor_au", "impor_us", "expor_au", "expor_us")

for(var in variables) {
  assign(var, log(get(var)))
}


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

#############################################
set.seed(123)
n <- 1000  # Number of observations
mu <- 0    # Mean
sigma <- 1 # Standard deviation

# Simulate two independent random walks
simulation_data <- data.frame(RW1 = cumsum(rnorm(n, mu, sigma)),RW2 = cumsum(rnorm(n, mu, sigma)))

plot(simulation_data$RW1, type = 'l', ylim = range(simulation_data), col = 'red', ylab = 'Value', xlab = 'Time', main = 'Bivariate Random Walk')
lines(simulation_data$RW2,col = 'blue')
legend("topright",legend = c("RW1", "RW2"), col = c("red", "blue"), lty = 1, cex = 0.6)

simulation_data <- data.frame(RW1 = cumsum(rnorm(n, mu, sigma)),RW2 = cumsum(rnorm(n, mu, sigma)))

plot(simulation_data$RW1, type = 'l', ylim = range(simulation_data), col = 'red', ylab = 'Value', xlab = 'Time', main = 'Bivariate Random Walk')
lines(simulation_data$RW2,col = 'blue')
legend("topright",legend = c("RW1", "RW2"), col = c("red", "blue"), lty = 1, cex = 0.6)

p=1

Y_simulation = (simulation_data[(p+1):nrow(simulation_data),]) #contains the obs of the two variables and moves first obs
X_simulation = matrix(1,nrow(Y_simulation),1) #initializes the X matrix with a column of ones(intercept) in the VAR model.
# adds the lagged values of the two variables to the X matrix, in this case, it adds one lagged value for each of the two variables.
for (i in 1:p){
  X_simulation     = cbind(X_simulation, (simulation_data[(p+1):nrow(simulation_data)-i,]))
}

Y_simulation = as.matrix(Y_simulation)
X_simulation = as.matrix(X_simulation)
####################################################################
y             = ts(merged_data[,1:ncol(merged_data )])
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

# Minnesota prior 
kappa.1       = 0.02^2                                    
kappa.2       = 100                                   
A.prior       = matrix(0,nrow(A.hat),ncol(A.hat))
A.prior[2:(N+1),] = diag(N) 
V.prior       = diag(c(kappa.2,kappa.1*((1:p)^(-2))%x%rep(1,N)))
S.prior       = diag(diag(Sigma.hat))
nu.prior      = N+1


posterior.draws       = function (S, Y, X, A.prior, V.prior, S.prior, nu.prior){
  
  # normal-inverse Wishard posterior parameters
  V.bar.inv         = t(X)%*%X + diag(1/diag(V.prior))
  V.bar             = solve(V.bar.inv)
  A.bar             = V.bar%*%(t(X)%*%Y + diag(1/diag(V.prior))%*%A.prior)
  nu.bar            = nrow(Y) + nu.prior
  S.bar             = S.prior + t(Y)%*%Y + t(A.prior)%*%diag(1/diag(V.prior))%*%A.prior - t(A.bar)%*%V.bar.inv%*%A.bar
  S.bar.inv         = solve(S.bar)
  
  # posterior draws 
  Sigma.posterior   = rWishart(S, df=nu.bar, Sigma=S.bar.inv)
  Sigma.posterior   = apply(Sigma.posterior,3,solve)
  Sigma.posterior   = array(Sigma.posterior,c(N,N,S))
  A.posterior       = array(rnorm(prod(c(dim(A.bar),S))),c(dim(A.bar),S))
  L                 = t(chol(V.bar))
  for (s in 1:S){
    A.posterior[,,s]= A.bar + L%*%A.posterior[,,s]%*%chol(Sigma.posterior[,,s])
  }
  
  output            = list(A.posterior=A.posterior, Sigma.posterior=Sigma.posterior)
  return(output)
}


h                      = 12
S                      = 10000
Y.h                    = array(NA,c(h,N,S))

## Applying function 
posterior.sample.draws = posterior.draws(S=10000, Y=Y, X=X,A.prior, V.prior, S.prior, nu.prior)
A.posterior.simu       = posterior.sample.draws$A.posterior
Sigma.posterior.simu   = posterior.sample.draws$Sigma.posterior

#######################plot pre-set up
sampling.predictive.density = function(A.posterior.simu, Sigma.posterior.simu, S) {
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
    return(Y.h)
}

Y.h       = sampling.predictive.density(A.posterior.simu = A.posterior.simu,
                                            Sigma.posterior.simu = Sigma.posterior.simu,
                                            S = S) 
basic.plot.2d.fun = function(Y.h){
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
}

basic.plot.2d = basic.plot.2d.fun(Y.h)


#################################plot data test 3D####
basic.plot.3d.fun = function(Y.h){
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
}

basic.plot.3d = basic.plot.3d.fun(Y.h)

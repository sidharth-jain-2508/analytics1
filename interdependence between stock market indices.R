# Download and Install required Packages from CRAN-like repositories or from local files
install.packages("quantmod")
install.packages("tseries")
install.packages("vars")
install.packages("xts")

# Loading/Attaching and Listing of Packages
library("quantmod")
library("tseries")
library("vars")
library("xts")

# Download data for the following indices from 2013-10-01 to 2018-09-30
# ^DJI - Dow Jones Industrial Average
# ^N225 - Nikkei 225
# ^HSI - Hang Seng Index
# ^NSEI - Nifty 50
# ^FTSE - FTSE 100 Index
indices = c("^DJI", "^N225", "^HSI", "^NSEI", "^FTSE")
getSymbols(indices, from = "2013-10-01", to = "2018-09-30")

# Select Adjusted Close Price and replacing each NA with the most recent non-NA prior to it
dji = na.locf(DJI$DJI.Adjusted)
n225 = na.locf(N225$N225.Adjusted)
hsi = na.locf(HSI$HSI.Adjusted)
nsei = na.locf(NSEI$NSEI.Adjusted)
ftse = na.locf(FTSE$FTSE.Adjusted)

# Take differential log and remove NA entries
dji_ret = na.omit(Delt(dji))
n225_ret = na.omit(Delt(n225))
hsi_ret = na.omit(Delt(hsi))
nsei_ret = na.omit(Delt(nsei))
ftse_ret = na.omit(Delt(ftse))

# Rename column names
colnames(dji_ret) <- "dji_ret"
colnames(n225_ret) <- "n225_ret"
colnames(hsi_ret) <- "hsi_ret"
colnames(nsei_ret) <- "nsei_ret"
colnames(ftse_ret) <- "ftse_ret"

# Merge the objects based on the common dates and remove NA entries
indices_ret = na.omit(merge(dji_ret, n225_ret, hsi_ret, nsei_ret, ftse_ret))
summary(indices_ret)
plot(indices_ret, nc=3, xlab="")

# Test the unit root in the series
adf.test(dji_ret, alternative="stationary", k=0)
adf.test(n225_ret, alternative="stationary", k=0)
adf.test(hsi_ret, alternative="stationary", k=0)
adf.test(nsei_ret, alternative="stationary", k=0)
adf.test(ftse_ret, alternative="stationary", k=0)

# To select the Lag order
VARselect(indices_ret, lag.max=10, type="both")

# AIC(n)  HQ(n)  SC(n) FPE(n) 
#   2      1      1      2 
# Select max lag found from VARselect
var = VAR(indices_ret, p=2, type="both")
var

# Acoef - Coefficient matrices of the lagged endogenous variables
# Bcoef - Coefficient matrix of an estimated VAR
# coef - is a generic function which extracts model coefficients
Acoef(var)
Bcoef(var)
coef(var)
summary(var)

# Computes the test statistics for Granger and Instantaneous causality for a VAR
causality(var, cause = "dji_ret", vcov.=NULL, boot=FALSE, boot.runs=100)
causality(var, cause = "n225_ret", vcov.=NULL, boot=FALSE, boot.runs=100)
causality(var, cause = "hsi_ret", vcov.=NULL, boot=FALSE, boot.runs=100)
causality(var, cause = "nsei_ret", vcov.=NULL, boot=FALSE, boot.runs=100)
causality(var, cause = "ftse_ret", vcov.=NULL, boot=FALSE, boot.runs=100)

# Time Series plots of VAR forecasts with differently shaded confidence regions (fanchart) for each endogenous variable.
varprd <- predict(var, n.ahead = 10, ci = 0.95)
par("mar")
par(mar=c(1.5,1.5,1.5,1.5))
fanchart(varprd)
x=predict(var, n.ahead = 10, ci = 0.95)
x                       

# Coefficient matrices of the orthogonalised Moving Average represention
Psi(var, nstep=5)

summary(var, equation="dji_ret")
plot(var, names="dji_ret")

summary(var, equation="n225_ret")
plot(var, names="n225_ret")

summary(var, equation="hsi_ret")
plot(var, names="hsi_ret")

summary(var, equation="nsei_ret")
plot(var, names="nsei_ret")

summary(var, equation="ftse_ret")
plot(var, names="ftse_ret")

# Plotting the impulse response functions graphs
imp = irf(var,n.ahead=10)
imp
plot(imp)

# Computes the forecast error variance decomposition of a VAR for 10 ahead steps
fev <- fevd(var,n.ahead=10)
fev
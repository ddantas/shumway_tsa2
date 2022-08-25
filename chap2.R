############################################################################
#
# shumway_tsa2/chap2.R
#
# ddantas 24/08/2022
#
############################################################################

source("const.R")


############################################################################
#
# Load functions
#
############################################################################

load_mort <- function()
{
  mort = scan(paste(DATA, "cmort.dat", sep=""))
  temp  = scan(paste(DATA, "temp.dat", sep=""))
  part  = scan(paste(DATA, "part.dat", sep=""))

  result = cbind(mort, temp, part)
  return(result)
}

load_detrend_globtemp <- function()
{
  temp = scan(paste(DATA, "globtemp.dat", sep=""))
  x = temp[45:142]
  x = ts(x, start=1900, frequency=1)
  dx = temp[44:142]
  dx = diff(dx)

  result = cbind(x, dx)
  return(result)
}

load_varve <- function()
{
  data = scan(paste(DATA, "varve.dat", sep=""))
  return(data) 
}

############################################################################
#
# Definitions
#
############################################################################


############################################################################
#
# Figures and examples
#
############################################################################

fig201 <- function()
{
  x = load_globtemp()
  s = start(x)[1]
  t = seq(s, s+length(x)-1)
  fit = lm(x~t)
  plot.ts(x, xlab="Year", ylab="Temp. deviation")
  abline(fit)
}

fig202 <- function()
{
  mat = load_mort()
  plot.ts(mat)
}

fig203 <- function()
{
  mat = load_mort()
  pairs(mat)
}

ex202 <- function()
{
  mat = load_mort()
  mort = mat[,"mort"]
  temp = mat[,"temp"]
  part = mat[,"part"]
  
  temp = temp - mean(temp)
  temp2 = temp^2
  t = 1:length(mort)
  fit1 = lm(mort~t)
  fit2 = lm(mort~t + temp)
  fit3 = lm(mort~t + temp + temp2)
  fit4 = lm(mort~t + temp + temp2 + part)

  return(list(fit1, fit2, fit3, fit4))
}

fig204 <- function()
{
  mat = load_detrend_globtemp()
  x  = mat[,"x"]
  dx = mat[,"dx"]  
  
  s = start(x)[1]
  t = seq(s, s+length(x)-1)
  fit = lm(x~t)
  par(mfrow=c(2, 1))
  plot(t, fit$resid, xlab="Year", ylab="Detrended gtemp", type="l")
  plot(t, dx, xlab="Year", ylab="Detrended gtemp", type="l")  
}

fig205 <- function()
{
  mat = load_detrend_globtemp()
  x  = mat[,"x"]
  dx = mat[,"dx"]  
  
  s = start(x)[1]
  t = seq(s, s+length(x)-1)
  fit = lm(x~t)
  par(mfrow=c(3, 1))
  lag = 50
  acf(x, lag)
  acf(fit$resid, lag)
  acf(dx, lag)
}

fig206 <- function()
{
  data = load_varve()

  par(mfrow=c(2, 1))
  plot.ts(data)
  result = log(data)
  plot.ts(result)
}

fig207 <- function()
{
  mat = load_soi()
  soi = mat[,"soi"]

  lag.plot(soi, lags=12, layout=c(3,4), fiag=F)
}

fig208 <- function()
{
  mat = load_soi()
  soi = mat[,"soi"]
  rec = mat[,"rec"]

  par(mfrow=c(4,4), mar=c(2.5, 4, 4, 1))
  for(h in 0:15)
  {
    plot(lag(soi, -h), rec, main=paste("SOI(t-", h, ")", sep=""),
         ylab="Rec(t)", xlab="")
  }
}

fig209 <- function()
{
  set.seed(154)
  w = rnorm(500, 0, 5)
  t = 1:500
  c = 2*cos(2*pi*t/50 + .6*pi)
  cw = c + w

  a = cos(2*pi*t/50)
  b = sin(2*pi*t/50)
  fit = lm(cw~a + b)

  par(mfrow=c(3,1))
  ts.plot(cbind(cw, c), col=c(1,2))
  plot.ts(a)
  plot.ts(b)

  A = sqrt(fit$coefficients[2]^2 + fit$coefficients[3]^2)
  phia = acos(fit$coefficients[2] / A) / pi
  phib = asin(-fit$coefficients[3] / A) / pi
  print(fit)
  print(A)
  print(phia)
  print(phib)
}

fig210 <- function()
{
  set.seed(154)
  w = rnorm(500, 0, 5)
  t = 1:500
  c = 2*cos(2*pi*t/50 + .6*pi)
  cw = c + w

  I = abs(fft(cw) / sqrt(500))^2
  P = 4/500 * I
  f = 0:250/500
  plot(f, P[1:251], type="l", xlab="frequency", ylab="")
  abline(v=seq(0, 0.5, 0.02), lty="dotted")
}

fig211 <- function()
{
  set.seed(154)
  n = 100
  w = rnorm(n, 0, 1)
  t = 1:n
  c = 2*cos(2*pi*t*(2/100))
  cw = c + w

  par(mfrow=c(3,1))
  s1 = 2*cos(2*pi*t*(1/100))
  main = paste("One cycle j/n=1/100 correlation=", cor(cw,s1), sep="")
  ts.plot(cbind(cw, s1), lty=c(1, 2), main=main)

  s2 = 2*cos(2*pi*t*(2/100))
  main = paste("Two cycles j/n=2/100 correlation=", cor(cw,s2), sep="")
  ts.plot(cbind(cw, s2), lty=c(1, 2), main=main)

  s3 = 2*cos(2*pi*t*(3/100))
  main = paste("Three cycles j/n=3/100 correlation=", cor(cw,s3), sep="")
  ts.plot(cbind(cw, s3), lty=c(1, 2), main=main)
}

fig212 <- function()
{
  mat = load_mort()
  mort = mat[,"mort"]
  ma5  = filter(mort, sides=2, rep(1,5)/5)
  ma53 = filter(mort, sides=2, rep(1,53)/53)

  plot(mort, col="gray", xlab="week", ylab="mortality")
  lines(ma5)
  lines(ma53)
}

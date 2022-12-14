############################################################################
#
# shumway_tsa2/chap1.R
#
# ddantas 22/08/2022
#
############################################################################

source("const.R")


############################################################################
#
# Load functions
#
############################################################################

load_eq5exp6 <- function()
{
  data = scan(paste(DATA, "eq5exp6.dat", sep=""))
  #len  = length(data)
  len2 = length(data) / 2
  #mat = matrix(0, len2, 2)
  #mat[,1] = data[1:len2]
  #mat[,2] = data[(len2+1):len]
  mat = array(data, dim=c(len2, 2))
  return(mat)
}

load_globtemp <- function()
{
  temp = scan(paste(DATA, "globtemp.dat", sep=""))
  temp = temp[45:142]
  temp = ts(temp, start=1900, frequency=1)
  return(temp)
}

load_soi <- function()
{
  soi = scan(paste(DATA, "soi.dat", sep=""))
  rec = scan(paste(DATA, "recruit.dat", sep=""))

  soi = ts(soi, start=1950, frequency=12)
  rec = ts(rec, start=1950, frequency=12)

  mat = cbind(soi, rec)
  return(mat)
}

load_jj <- function()
{
  jj = scan(paste(DATA, "jj.dat", sep=""))
  jj = ts(jj, start=1960, frequency=4)
  return(jj)
}


############################################################################
#
# Definitions
#
############################################################################

# Definition 1.14: sample autocovariance function
sacov <- function(x, h)
{
  mx = mean(x)
  nx = length(x)
  result = mean((x[(1+h):nx] - mx) * (x[1:(nx-h)] - mx))
  return(result)
}

# Definition 1.15: sample autocorrelation function
sacor <- function(x, h)
{
  result = sacov(x, h) / sacov(x, 0)
  return(result)
}

# Definition 1.16: sample cross-covariance function
sccov <- function(x, y, h)
{
  nx = length(x)
  ny = length(y)
  if (nx != ny)
  {
    writeLines("Error: chap1.R: sccov: length(x) != length(y)")
    return
  }
  mx = mean(x)
  my = mean(y)
  if (h >= 0)
  {
    result = mean((x[(1+h):nx] - mx) * (y[1:(ny-h)] - my))
    return(result)
  }
  result = mean((x[(1):(nx+h)] - mx) * (y[(1-h):(ny)] - my))
  return(result)
}

# Equation 1.47: sample autocovariance matrix
sacovmat <- function(mat, h)
{
  if (length(dim(mat)) !=2)
  {
    writeLines("Error: chap1.R: sacovmat: length(x) != length(y)")
    return
  }
  ncols = dim(mat)[2]
  result = matrix(0, ncols, ncols)
  for (i in seq(1, ncols))
  {
    for (j in seq(1, ncols))
    {
      result[i, j] = sccov(mat[,i], mat[,j], h)
    }
  }
  return(result)
}


############################################################################
#
# Figures and examples
#
############################################################################

# Figure 1.1: J&J quarterly EPS, 84 quarters 1960-I to 1980-IV.
fig101 <- function()
{
  jj = load_jj()
  plot(jj, ylab="Quarterly earnings per share", pch=PCH, type=TYPE)
}

# Figure 1.2: Yearly average global temperature deviations (1900-1997) in degrees centigrade.
fig102 <- function()
{
  temp = load_globtemp()
  plot(temp, ylab="Global temperature deviation", pch=PCH, type=TYPE)
}

# Figure 1.3: Speech recording of the syllable aa..hh sampled at 10kHz with n=1020 points.
fig103 <- function()
{
  sp = scan(paste(DATA, "speech.dat", sep=""))
  plot(sp, ylab="Speech recording", type="l")
}

# Figure 1.4: Returns ofthe NYSE. The data are daily value weighted market returns from Feb 2, 1984 to Dec 31, 1991 (2000 trading days).
fig104 <- function()
{
  data = scan(paste(DATA, "nyse.dat", sep=""))
  plot(data, ylab="NYSE returns", type="l")
}

# Figure 1.5: Monthly SOI and recruitment, 1950-1987.
fig105 <- function()
{
  mat = load_soi()
  soi = mat[,"soi"]
  rec = mat[,"rec"]
  
  par(mfrow=c(2, 1))
  plot.ts(soi, main="SOI")
  plot.ts(rec, main="Recruits")
}

# Figure 1.6: fMRI data from various locations in the cortex, thalamus, and cerebellum; n=128 points at 0.5 Hz.
fig106 <- function()
{
  data = read.table(paste(DATA, "fmri.dat", sep=""))

  par(mfrow=c(2, 1))
  ts.plot(data[,2:5], lty=c(1,4), ylab="BOLD", main="Cortex")
  ts.plot(data[,6:9], lty=c(1,4), ylab="BOLD", main="Thalamus & Cerebellum")
}

# Figure 1.7: Arrival phases from an earthquake and explosion at 40Hz.
fig107 <- function()
{
  mat = load_eq5exp6()
  
  par(mfrow=c(2, 1))
  plot.ts(mat[,1], main="Earthquake", ylab="EQ5")
  plot.ts(mat[,2], main="Explosion", ylab="EXP6")
}

# Figure 1.8: Gaussian white noise series and its three-point moving average.
fig108 <- function()
{
  w = rnorm(500, 0, 1)
  v = filter(w, sides=2, rep(1,3)/3)
  
  par(mfrow=c(2, 1))
  plot.ts(w)
  plot.ts(v)
}

# Figure 1.9: Autoregressive series generated from model 1.2.
fig109 <- function()
{
  w = rnorm(550, 0, 1)
  v = filter(w, filter=c(1, -0.9), method="recursive")
  
  par(mfrow=c(2, 1))
  plot.ts(w)
  plot.ts(v)
}

# Figure 1.10: Random walk, sigma = 1, with drift delta = .2.
fig110 <- function()
{
  set.seed(154)
  w = rnorm(200, 0, 1)
  x = cumsum(w)
  wd = w +.2
  xd = cumsum(wd)
  plot.ts(xd, ylim=c(-5, 55))
  lines(x)
  lines(0.2 * (1:200), lty="dashed")
}

# Figure 1.11: Cosine wave with period 50 compared with the cosine wave contaminated with addictive white Gaussian noise.
fig111 <- function()
{
  set.seed(154)
  t = 1:500
  c = 2*cos(2*pi*t/50 + .6*pi)
  w = rnorm(500, 0, 1)
  par(mfrow=c(3, 1))
  plot.ts(c)
  plot.ts(c + w)
  plot.ts(c + 5*w)
}

# Example 1.16: Autocovariance of white noise.
ex116 <- function()
{
  w = rnorm(500, 0, 1)
  par(mfrow=c(2, 1))
  plot.ts(w)
  acf(w, type="covariance")
}

# Example 1.16: Autocovariance of a moving average.
ex117 <- function(n=500)
{
  w = rnorm(n, 0, 1)
  v = filter(w, sides=2, rep(1,3)/3)
  v[is.na(v)] = w[is.na(v)]
  par(mfrow=c(2, 1))
  plot.ts(v)
  acf(v, type="covariance")
}

# Property 1.1: Large sample distribution of the autocorrelation function
prop101 <- function(n, H)
{
  x = rnorm(n, 0, 1)
  result = seq(H)
  for (i in seq(H))
  {
    result[i] = sacor(x, i)
  }
  sdr = sd(result)
  result = 1.0 / (sdr * sdr)
  print(paste("n =", n))
  print(paste("1.0 / sd(sacor)^2 =", result))
}

# Figure 1.13: ACF of the speech series.
fig113 <- function()
{
  sp = scan(paste(DATA, "speech.dat", sep=""))
  par(mfrow=c(2, 1))
  plot.ts(sp)
  acf(sp, 250)
}

# Figure 1.14: Sample ACFs of the SOI seriesand of the recruitment series.
fig114 <- function()
{
  data1 = scan(paste(DATA, "soi.dat", sep=""))
  data2 = scan(paste(DATA, "recruit.dat", sep=""))

  #data1 = ts(data1, start=1950, frequency=12)
  #data2 = ts(data2, start=1950, frequency=12)
  
  par(mfrow=c(3, 2))
  plot.ts(data1, main="SOI")
  plot.ts(data2, main="Recruits")
  acf(data1, 50)
  acf(data2, 50)
  ccf(data1, data2, 50)
}

# Figure 1.15: Two-dimensional time series of temperature measurements taken on a rectangular field (64x37 with 17-foot spacing).
fig115 <- function()
{
  data = scan(paste(DATA, "soiltemp.dat", sep=""))
  mat = t(array(data, dim=c(36, 64)))
  persp(mat, zlab="Temperature", xlab="Rows", ylab="Columns", theta=210, phi=30, scale=TRUE, expand=0.3)
}

# Figure 1.16: Row averages of the two-dimensional soil temperature profile.
fig116 <- function()
{
  data = scan(paste(DATA, "soiltemp.dat", sep=""))
  mat = t(array(data, dim=c(36, 64)))
  result = rowMeans(mat)
  plot.ts(result)
}


############################################################################
#
# Problems
#
############################################################################

prob101 <- function()
{
  mat = load_eq5exp6()

  ts.plot(mat[,1:2], lty=c(1,2), col=c(1,2))
}

prob102 <- function()
{
  mat = matrix(0, 200, 4)

  mat[101:200,1] = 10*exp(-seq(100)/20.0)  * cos(2*pi*seq(101:200)/4.0)
  mat[101:200,2] = 10*exp(-seq(100)/200.0) * cos(2*pi*seq(101:200)/4.0)
  mat[101:200,3] = 10*exp(-seq(100)/20.0)
  mat[101:200,4] = 10*exp(-seq(100)/200.0)

  ts.plot(mat[,1:4], lty=c(1,2), col=c(1,2))
  return(mat)
}

prob103 <- function()
{
  set.seed(154)
  wa = rnorm(100, 0, 1)
  xa = filter(wa, filter=c(0.0, -0.9), method="recursive")
  va = filter(xa, filter=c(0.25, 0.25, 0.25, 0.25), method="recursive")

  xb = cos(2*pi*seq(100)/8.0)
  vb = filter(xb, filter=c(0.25, 0.25, 0.25, 0.25), method="recursive")

  xc = cos(2*pi*seq(100)/8.0) + wa
  vc = filter(xc, filter=c(0.25, 0.25, 0.25, 0.25), method="recursive")

  mata = cbind(xa, va)
  matb = cbind(xb, vb)
  matc = cbind(xc, vc)
  
  par(mfrow=c(3, 1))
  ts.plot(mata[,1:2], lty=c(1,2), col=c(1,2))
  ts.plot(matb[,1:2], lty=c(1,2), col=c(1,2))
  ts.plot(matc[,1:2], lty=c(1,2), col=c(1,2))
}

prob105 <- function()
{
  mat = prob102()
  mat_sacov = mat[,1:2]

  print(mean(mat[,1]))
  print(mean(mat[,2]))

  for (j in seq(1:2))
  {
    for (i in seq(0:199))
    {
      result = sacov(mat[,j], i)
      mat[i,j] = result
      #cat(j, "\t", i, "\t", result, "\n", sep="\t")
    }
  }
  
  par(mfrow=c(2, 1))
  ts.plot(mat[,c(1,3)], lty=c(1,2), col=c(1,2))
  lines(mat_sacov[,1])
  ts.plot(mat[,c(2,4)], lty=c(1,2), col=c(1,2))
  lines(mat_sacov[,2])
}

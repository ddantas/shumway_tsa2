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
  cmort = scan(paste(DATA, "cmort.dat", sep=""))
  temp  = scan(paste(DATA, "temp.dat", sep=""))
  part  = scan(paste(DATA, "part.dat", sep=""))

  result = cbind(cmort, temp, part)
  return(result)
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


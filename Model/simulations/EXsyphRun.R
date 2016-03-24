library("deSolve")
library("reshape2")
library("ggplot2"); theme_set(theme_bw())
source("../syphFuns.R")
source("../syphParms.R")
source("../syphModels.R")

r <- lsoda(unlist(yini), func = gfun(pars.mean), parms = pars.mean, times = c(1:200))

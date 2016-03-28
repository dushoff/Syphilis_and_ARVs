library("deSolve")
library("reshape2")
library("ggplot2"); theme_set(theme_bw())
source("../syphFuns.R")
source("../syphParms.R")
source("../syphModels.R")

r <- lsoda(unlist(calc_yini(pars.mean)), func = gfun(pars.mean), parms = pars.mean, times = tvec)
plot(r)

r2 <- lsoda(unlist(calc_yini(pars.mean, syph = TRUE)), func = gfun(pars.mean, syph = TRUE), parms = pars.mean, times = tvec)
plot(r2)


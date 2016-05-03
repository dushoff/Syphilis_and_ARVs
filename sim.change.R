library("deSolve")
source("base.parms.R")
source("model.R")
source("functions.R")
source("simFuns.R")

base.pars <- as.parlist(Pars.skeleton)

tvec <- seq(0,80,by=0.01)

yini.co <- unlist(calc_yini(base.pars, type = 1))

syph_sim.base <- lsoda(yini.co, func = gfun.change(base.pars), parms = base.pars, times = tvec)
change.pars <- transform(base.pars, nu_is = 3)
syph_sim.change <- lsoda(yini.co, func = gfun.change(change.pars), parms = change.pars, times = tvec)
noARV.pars <- transform(base.pars, tau = 0)
syph_sim.noARV <- lsoda(yini.co, func = gfun(noARV.pars), parms = noARV.pars, times = tvec)

I.base <- rowSums(syph_sim.base[,8:13])/rowSums(syph_sim.base[,-1])
I.change <- rowSums(syph_sim.change[,8:13])/rowSums(syph_sim.change[,-1])
I.noARV <- rowSums(syph_sim.noARV[,8:13])/rowSums(syph_sim.noARV[,-1])

matplot(cbind(I.base, I.change, I.noARV), type = "l")

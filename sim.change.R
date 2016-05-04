library("deSolve")
source("base.parms.R")
source("model.R")
source("functions.R")
source("simFuns.R")

base.pars <- as.parlist(Pars.skeleton)

tvec <- seq(0,60,by=0.01)

yini.co <- unlist(calc_yini(base.pars, type = 1))

noARV.pars <- transform(base.pars, tau = 0)
syph_sim.noARV <- lsoda(yini.co, func = gfun.change(noARV.pars), parms = noARV.pars, times = tvec)
ARVonly.pars <- transform(base.pars, nu_is = 3)
syph_sim.ARVonly <- lsoda(yini.co, func = gfun.change(ARVonly.pars), parms = ARVonly.pars, times = tvec)
behave.pars <- transform(base.pars, pc.increase = 3)
syph_sim.behave <- lsoda(yini.co, func = gfun.change(behave.pars), parms = behave.pars, times = tvec)
both.pars <- transform(base.pars, nu_is = 3, pc.increase = 3)
syph_sim.both <- lsoda(yini.co, func = gfun.change(both.pars), parms = both.pars, times = tvec)

save("syph_sim.noARV", "syph_sim.ARVonly", "syph_sim.behave", "syph_sim.both", file = "syphData2.rda")

I.noARV <- rowSums(syph_sim.noARV[,8:13])/rowSums(syph_sim.noARV[,-1])
I.ARVonly <- rowSums(syph_sim.ARVonly[,8:13])/rowSums(syph_sim.ARVonly[,-1])
I.behave <- rowSums(syph_sim.behave[,8:13])/rowSums(syph_sim.behave[,-1])
I.both <- rowSums(syph_sim.both[,8:13])/rowSums(syph_sim.both[,-1])

matplot(cbind(I.noARV, I.ARVonly, I.behave, I.both), type = "l")

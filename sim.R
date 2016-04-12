library("deSolve")

base.pars <- as.parlist(Pars.skeleton)

tvec <- seq(0,80,by=0.01)

yini.co <- unlist(calc_yini(base.pars, type = 1))
yini.HIV <- unlist(calc_yini(base.pars, type = 2))
yini.syph <- unlist(calc_yini(base.pars, type = 3))

syph_sim.co <- lsoda(yini.co, func = gfun(base.pars), parms = base.pars, times = tvec)
syph_sim.HIV <- lsoda(yini.HIV, func = gfun(base.pars), parms = base.pars, times = tvec)
syph_sim.syph <- lsoda(yini.syph, func = gfun(base.pars), parms = base.pars, times = tvec)

save("syph_sim.co", "syph_sim.HIV", "syph_sim.syph", file = "syphData.rda")

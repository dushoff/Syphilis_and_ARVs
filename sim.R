library("deSolve")

base.pars <- as.parlist(Pars.skeleton)

tvec <- seq(0,80,by=0.01)

yini <- unlist(calc_yini(base.pars, syph = TRUE))

syph_sim2 <- lsoda(yini, func = gfun(base.pars), parms = base.pars, times = tvec)

save("syph_sim2", file = "syphData.rda")

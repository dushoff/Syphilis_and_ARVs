
library("deSolve")

base.pars <- as.parlist(Pars.skeleton)

tvec <- seq(0,60,by=0.01)

yini.co <- unlist(calc_yini(base.pars, type = 1))
example.pars <- transform(base.pars, pc.increase = 3, nu_is = 3, behave.start = 20, T_c = 1)
syph_sim.ex <- lsoda(yini.co, func = gfun.change2(example.pars), parms = example.pars, times = tvec)


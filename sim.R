library("deSolve")

base_sim <- lsoda(unlist(calc_yini(pars.mean)), func = g.base, parms = pars.mean, times = tvec)

syph_sim <- lsoda(unlist(calc_yini(pars.mean, syph = TRUE)), func = g.syph, parms = pars.mean, times = tvec)

syph_sim2 <- lsoda(unlist(calc_yini(pars.mean, syph = TRUE)), func = gfun(pars.mean), parms = pars.mean, times = tvec)
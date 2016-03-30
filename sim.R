library("deSolve")

base_sim <- lsoda(unlist(calc_yini(pars.mean)), func = gfun(pars.mean), parms = pars.mean, times = tvec)

syph_sim <- lsoda(unlist(calc_yini(pars.mean, syph = TRUE)), func = gfun(pars.mean, syph = TRUE), parms = pars.mean, times = tvec)


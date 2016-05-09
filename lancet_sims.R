library("deSolve")
source("base.parms.R")
source("model.R")
source("functions.R")
source("simFuns.R")

base.pars <- as.parlist(Pars.skeleton)

tvec <- seq(0,60,by=0.01)
behave.start <- 20

yini.co <- unlist(calc_yini(base.pars, type = 1))

base.only.sim <- lsoda(yini.co
	, func = gfun.change2(base.pars)
	, parms = base.pars
	, times = tvec
)

slow.pars = transform(base.pars
	, pc.increase = 3, behave.start = behave.start, T_c = 3
) 
base.slow.sim <- lsoda(yini.co
	, func = gfun.change2(slow.pars), parms=slow.pars
	, times = tvec
)

fast.pars = transform(base.pars
	, pc.increase = 3, behave.start = behave.start, T_c = 1
) 
base.fast.sim <- lsoda(yini.co
	, func = gfun.change2(fast.pars), parms=fast.pars
	, times = tvec
)

susc.pars <- transform(base.pars, nu_is = 3)
susc.only.sim <- lsoda(yini.co
	, func = gfun.change2(susc.pars)
	, parms = susc.pars
	, times = tvec
)

slow.pars = transform(susc.pars
	, pc.increase = 3, behave.start = behave.start, T_c = 3
) 
susc.slow.sim <- lsoda(yini.co
	, func = gfun.change2(slow.pars), parms=slow.pars
	, times = tvec
)

fast.pars = transform(susc.pars
	, pc.increase = 3, behave.start = behave.start, T_c = 1
) 
susc.fast.sim <- lsoda(yini.co
	, func = gfun.change2(fast.pars), parms=fast.pars
	, times = tvec
)

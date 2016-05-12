library("deSolve")

base.pars <- as.parlist(Pars.skeleton)

tvec <- seq(0,40,by=0.01)
behave.start <- base.pars$ARV.start

yini.co <- unlist(calc_yini(base.pars, type = 1))

base.only.sim <- lsoda(yini.co
	, func = g.syph
	, parms = base.pars
	, times = tvec
)

slow.pars = transform(base.pars
	, pc.increase = 3, behave.start = behave.start, T_c = 3
) 

base.slow.sim <- lsoda(yini.co
	, func = g.syph
	, parms = slow.pars
	, times = tvec
)

fast.pars = transform(base.pars
	, pc.increase = 3, behave.start = behave.start, T_c = 1
) 
base.fast.sim <- lsoda(yini.co
	, func = g.syph
	, parms = fast.pars
	, times = tvec
)

susc.pars <- transform(base.pars, nu_is = 3)
susc.only.sim <- lsoda(yini.co
	, func = g.syph
	, parms = susc.pars
	, times = tvec
)

slow.pars = transform(susc.pars
	, pc.increase = 3, behave.start = behave.start, T_c = 3
) 
susc.slow.sim <- lsoda(yini.co
	, func = g.syph
	, parms = slow.pars
	, times = tvec
)

fast.pars = transform(susc.pars
	, pc.increase = 3, behave.start = behave.start, T_c = 1
) 

susc.fast.sim <- lsoda(yini.co
	, func = g.syph
	, parms = fast.pars
	, times = tvec
)

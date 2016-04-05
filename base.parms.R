n.trial <- 1000  ## total number of sims
set.seed(101)

Pars.skeleton <- list(
  c = c(40, 5),	##partnership change rate [1]
  eps_b = 0.4, ##decreased transmission ratio due to ART [2]
  eps_a = 0.5, ##decreased progression rate due to ART [assumption]
  tau = 1, ##ART treatment rate [2]
  sigma = 0.015, ##Leaving ART [2]
  beta1 = 0.627, ##Transmission rate of primary stage(syph) [1]
  beta2 = 0.618, ##Transmission rate of secondary stage(syph) [1]
  D1 = 46/365, ##Mean duration of primary stage(syph) [1]
  D2 = 108/365, ##Mean duration of secondary stage(syph) [1]
  delta = 1/5, ##Rate at which immunity is lost [1]
  p = 0.25, ##proportion entering susceptible after treatment [assumption]
  iniI = 0.001,
  N0 = c(0.2,0.8)
)

##sources

##[1] Garnett et al. 1997
##[2] Granich et al. 2009
Pars.skeleton <- list(
  c = c(40, 5),	##partnership change rate [1]
  mu = 0.05, ##rate of entry/exit from at risk population [1]
  eps_b = 0.04, ##decreased transmission (ratio) due to ART [3]
  eps_a = 0.5, ##decreased progression rate (ratio) due to ART [4]
  tau = 1, ##ART treatment rate [2]
  sigma = 0.015, ##Leaving ART [2]
  beta = 0.6, ##Transmission probability per partnership [1]
  gamma = 6, ##Syphilis treatment rate [5]
  delta = 1/5, ##Rate at which immunity is lost [1]
  p = 0.25, ##proportion entering susceptible after treatment [assumption]
  iniI = 0.001, ##initial proportion of infected [asusmption]
  N0 = c(0.2,0.8) ##proportion of population group [assumption]
)

##sources

##[1] Garnett et al. 1997
##[2] Granich et al. 2009
##[3] Cohen et al. 2011
##[4] HIV-Causal Collaboration. 2010
##[5] Grassly et al. 2004
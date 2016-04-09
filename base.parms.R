## All rates are per year. All ratios and probabilities are unitless

Pars.skeleton <- list(
  c = c(40, 5),	## partnership change rate [1]
  mu = 0.05, ## rate of entry/exit from at risk population [1]
  c_w = 0.097, ## HIV transmission probability per partnership [6]
  eps_b = 0.04, ## relative HIV transmission ratio of people on ART [3]
  alpha.H = 0.125, ## HIV induced mortaility [7]
  eps_a = 0.5, ## relative mortality ratio of people on ART [4]
  rho = 0.3, ##proportion of non-random contact [assumption]
  tau = 1, ##ART treatment rate [2]
  sigma = 0.015, ## ART failure/loss rate [2]
  beta = 0.6, ##Syphilis transmission probability per partnership [1]
  gamma = 6, ##Syphilis treatment rate [5]
  delta = 1/5, ##Rate at which syphilis immunity is lost [1]
  p = 0., ## proportion treated for syphilis who return immediately to susceptible [5]
  nu_t = 1, ## relative HIV transmission ratio of people who are infected with syphilis [assumption]
  nu_r = 1, ## relative HIV receiving ratio of people who are infected with syphilis [assumption]
  iniI = 0.001, ##initial proportion of infected [assumption]
  N0 = c(0.2,0.8) ##proportion of population group [assumption]
)

## Move parameters that are elsewhere (LHS file, check if there are any in model file)

## Add interaction parameters (you can set them to 1 for now, or use values from Bob's email)

##sources

##[1] Garnett et al. 1997
##[2] Granich et al. 2009
##[3] Cohen et al. 2011
##[4] HIV-Causal Collaboration. 2010
##[5] Grassly et al. 2004
##[6] Grant et al. 1987
##[7] Champredon et al. 2014
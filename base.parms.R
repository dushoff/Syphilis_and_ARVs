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
  delta = 0.05, ##Rate at which syphilis immunity is lost [5]
  p = 0, ## proportion treated for syphilis who return immediately to susceptible [5]
  nu_t = 2, ## relative HIV transmission ratio of people who are infected with syphilis [8]
  nu_r = 3, ## relative HIV receiving ratio of people who are infected with syphilis [9]
  nu_is = 1, ## relative syphilis acquiring ratio due to ARV immunosuppression [assumption]
  nu_HIV = 1, ## relative syphilis transmission ratio of people who are infected with HIV [assumption]
  nu_ARV = 1, ## relative syphilis transmission ratio of people who are being treated with ARV [assumption]
  iniI = 0.001, ##initial proportion of infected [assumption]
  N0 = c(0.1,0.9) ##proportion of population group [assumption]
)

##sources

##[1] Garnett et al. 1997
##[2] Granich et al. 2009
##[3] Cohen et al. 2011
##[4] HIV-Causal Collaboration. 2010
##[5] Grassly et al. 2004
##[6] Grant et al. 1987
##[7] Champredon et al. 2014
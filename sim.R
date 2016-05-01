library("deSolve")
source("base.parms.R")
source("model.R")
source("functions.R")
source("simFuns.R")

base.pars <- as.parlist(Pars.skeleton)

tvec <- seq(0,80,by=0.01)

yini.co <- unlist(calc_yini(base.pars, type = 1))
yini.HIV <- unlist(calc_yini(base.pars, type = 2))
yini.syph <- unlist(calc_yini(base.pars, type = 3))

syph_sim.co <- lsoda(yini.co, func = gfun(base.pars), parms = base.pars, times = tvec)
syph_sim.HIV <- lsoda(yini.HIV, func = gfun(base.pars), parms = base.pars, times = tvec)
syph_sim.syph <- lsoda(yini.syph, func = gfun(base.pars), parms = base.pars, times = tvec)

save("syph_sim.co", "syph_sim.HIV", "syph_sim.syph", file = "syphData.rda")

titrationSim <- list()
titrationSimN <- 10
titrationSyphSPrev <- matrix(NA, nrow = length(tvec), ncol = titrationSimN)
titrationSyphIPrev <- matrix(NA, nrow = length(tvec), ncol = titrationSimN)
titrationSyphTPrev <- matrix(NA, nrow = length(tvec), ncol = titrationSimN)

nuVec <- seq(from = 1.1, to = 2, length.out = 10)

for(i in 1:titrationSimN){
	titration.pars <- transform(base.pars, nu_is = nuVec[i])
	titrationSim[[i]] <- lsoda(yini.co, func = gfun(titration.pars), parms = titration.pars, times = tvec)
	titrationSyphIPrev[,i] <- rowSums(titrationSim[[i]][,8:13])/rowSums(titrationSim[[i]][,-1])
	titrationSyphSPrev[,i] <- rowSums(titrationSim[[i]][,2:7])/rowSums(titrationSim[[i]][,-1])
	titrationSyphTPrev[,i] <- rowSums(titrationSim[[i]][,14:19])/rowSums(titrationSim[[i]][,-1])
}

save("titrationSim", "titrationSyphSPrev", "titrationSyphIPrev", "titrationSyphTPrev", file = "titration.rda")


library("deSolve")

n.trial <- 1000  ## total number of sims
set.seed(101)

Pars.range = read.table("Pars.range.txt", head = TRUE, row.name = 1)
pars.mean <- as.parlist(apply(Pars.range, 1, geom_mean))

## Non-randomized LHS data frame
ltab <- as.data.frame(apply(
	Pars.range, 1, function(x){
		exp(seq(log(x[1]),log(x[2]), length=n.trial))
	}
))

colnames(ltab) <- Pars.range[,1]

# Randomize and re-data-frame
ltab[] <- lapply(ltab,sample)

tvec <- seq(0,80,by=0.01)

syph_sim2 <- lsoda(unlist(calc_yini(pars.mean, syph = TRUE)), func = gfun(pars.mean), parms = pars.mean, times = tvec)

save("syph_sim2", file = "syphData.rda")

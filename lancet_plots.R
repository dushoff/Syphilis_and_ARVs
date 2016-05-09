library(deSolve)

library("ggplot2"); theme_set(theme_classic())
library("gridExtra")
library("plyr")  ## for ldply()
library("reshape2")  ## for melt()
library("GGally")

syphInc <- function(dat, tmin=0){
	df <- as.data.frame(dat)
	df <- subset(df, time>tmin)
	return(data.frame(
		time = df$time, 
		Prevalence = rowSums(df[,8:13])/rowSums(df[,2:19])
	))
}

scenFrame <- function(simList){
	prevList <- lapply(simList, syphInc)
	prevFrame <- melt(prevList, id.vars = "time")
	prevFrame$scenario <- factor(prevFrame$L1)
	levels(prevFrame$scenario) = c(
		"No behaviour change", "Slow change", "Fast change"
	)
	return(prevFrame)
}

framePlot <- function(f, ftitle){
	return((ggplot(f , aes(time, value, col = scenario))
		+ geom_line()
		+ ggtitle(ftitle)
		+ labs(x = "Time (years)", y = "Prevalence")
		+ scale_y_continuous(limits = c(0,0.09))
		+ scale_color_manual(
			values=c("black", "red", "blue")
		)
	))
}


baseFrame <- scenFrame(list(base.only.sim, base.slow.sim, base.fast.sim))
bp <- framePlot(baseFrame, "No biological response")

suscFrame <- scenFrame(list(susc.only.sim, susc.slow.sim, susc.fast.sim))
sp <- framePlot(baseFrame, "Increased susceptibility")


print(bp)
base.only.sim <- as.data.frame(base.only.sim)
base.slow.sim <- as.data.frame(base.slow.sim)
base.fast.sim <- as.data.frame(base.fast.sim)

base.slow.sim <- subset(base.slow.sim, time>behave.start)
base.fast.sim <- subset(base.fast.sim, time>behave.start)
base.only.sim <- as.data.frame(base.only.sim)
base.slow.sim <- as.data.frame(base.slow.sim)
base.fast.sim <- as.data.frame(base.fast.sim)

base.slow.sim <- subset(base.slow.sim, time>behave.start)
base.fast.sim <- subset(base.fast.sim, time>behave.start)
# print(grid.arrange(bp, sp, nrow = 1))

library(deSolve)

library("ggplot2"); theme_set(theme_classic())
library("gridExtra")
library("plyr")  ## for ldply()
library("reshape2")  ## for melt()
library("GGally")

syphInc <- function(dat){
	df <- as.data.frame(dat)
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
		"None", "Slow", "Fast"
	)
	return(prevFrame)
}

framePlot <- function(f, ftitle){
	f <- subset(f, L1==1 | time > behave.start)
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
sp <- framePlot(suscFrame, "Increased susceptibility")

print(bp)
print(sp)

library("deSolve")
source("base.parms.R")
source("model.R")
source("functions.R")
source("simFuns.R")

library("ggplot2"); theme_set(theme_classic())
scale_colour_discrete <- function(...,palette="Set1") scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Set1") scale_fill_brewer(...,palette=palette)
library("gridExtra")
library("plyr")  ## for ldply()
library("reshape2")  ## for melt()
library("GGally")
## devtools::install_github("lionel-/ggstance")
library("ggstance")

base.pars <- as.parlist(Pars.skeleton)

tvec <- seq(0,60,by=0.01)

yini.co <- unlist(calc_yini(base.pars, type = 1))

ARVonly.pars <- transform(base.pars, nu_is = 3)
syph_sim.ARVonly <- lsoda(yini.co, func = gfun.change2(ARVonly.pars), parms = ARVonly.pars, times = tvec)
behave.pars1 <- transform(base.pars, pc.increase = 3, behave.start = 20, T_c = 1) ##5
syph_sim.behave1 <- lsoda(yini.co, func = gfun.change2(behave.pars1), parms = behave.pars1, times = tvec)
behave.pars2 <- transform(base.pars, pc.increase = 3, behave.start = 20, T_c = 2) ##10
syph_sim.behave2 <- lsoda(yini.co, func = gfun.change2(behave.pars2), parms = behave.pars2, times = tvec)
behave.pars3 <- transform(base.pars, pc.increase = 3, behave.start = 20, T_c = 3) ##20
syph_sim.behave3 <- lsoda(yini.co, func = gfun.change2(behave.pars3), parms = behave.pars3, times = tvec)

syphList <- list()
stateNames <- c("ARVonly", "behave1", "behave2", "behave3")

for(m in stateNames){
	
	data <- get(paste0("syph_sim.",m))
	
	syphList[[m]]<- data.frame(
		tvec,
		rowSums(data[,8:13])/rowSums(data[,2:19])
	)	
	names(syphList[[m]]) <- c("time", "Prevalence")
}

sumList1 <- melt(syphList, id.vars = "time")

sumList1$L1 <- factor(sumList1$L1, levels = c("ARVonly", "behave3", "behave2", "behave1"))
	
p1 <- ggplot(sumList1, aes(time, value, col = L1)) + geom_line() +
	ggtitle("Independent effects of ARV induced syphilis susceptibility \nand behaviour change on prevalence") +
	theme(plot.title = element_text(size = 14, hjust = 0, margin = margin(0,0,15,0)),
		legend.position=c(0.73,0.85),
		legend.text = element_text(size = 11)) +
	labs(x = "Time (years)", y = "Prevalence") + scale_y_continuous(limits = c(0,0.09)) +
	scale_color_manual(values=c("red", "blue", "green", "black"),
		breaks = c("ARVonly","behave1", "behave2", "behave3"),
		labels = c("Behaviour change is not introduced", "5",
			"10", "20"),
		name = "Amount of time it takes for the \nmagnitude of behaviour change \nto reach the maximum value (years)")

both.pars1 <- transform(base.pars, pc.increase = 3, nu_is = 3, behave.start = 20, T_c = 1)
syph_sim.both1 <- lsoda(yini.co, func = gfun.change2(both.pars1), parms = both.pars1, times = tvec)
both.pars2 <- transform(base.pars, pc.increase = 3, nu_is = 3, behave.start = 20, T_c = 2)
syph_sim.both2 <- lsoda(yini.co, func = gfun.change2(both.pars2), parms = both.pars2, times = tvec)
both.pars3 <- transform(base.pars, pc.increase = 3, nu_is = 3, behave.start = 20, T_c = 3)
syph_sim.both3 <- lsoda(yini.co, func = gfun.change2(both.pars3), parms = both.pars3, times = tvec)

syphList2 <- list()
stateNames2 <- c("both1","both2", "both3")

for(m in stateNames2){
	
	data <- get(paste0("syph_sim.",m))
	
	syphList2[[m]]<- data.frame(
		tvec,
		rowSums(data[,8:13])/rowSums(data[,2:19])
	)	
	names(syphList2[[m]]) <- c("time", "Prevalence")
}

sumList2 <- melt(syphList2, id.vars = "time")

sumList2$L1 <- factor(sumList2$L1, levels = c("both3", "both2", "both1"))

p2 <- ggplot(sumList2, aes(time, value, col = L1)) + geom_line() +
	ggtitle("Combined effects of ARV induced syphilis susceptibility \nand behaviour change on prevalence") +
	theme(plot.title = element_text(size = 14, hjust = 0, margin = margin(0,0,15,0)),
		legend.position=c(0.73,0.87),
		legend.text = element_text(size = 11)) +
	labs(x = "Time (years)", y = " ") + scale_y_continuous(limits = c(0,0.09)) +
	scale_color_manual(values=c("blue", "green", "black"),
		breaks = c("both1", "both2", "both3"),
		labels = c("5", "10", "20"),
		name = "Amount of time it takes for the \nmagnitude of behaviour change \nto reach the maximum value (years)")

save("sumList1", "sumList2", file = "plotSimple.rda")

plot <- grid.arrange(p1,p2, nrow = 1)

##Don't need to look at things after this line

#############################################################################################
ggsave(plot, file="finalPlot2.jpg", dpi = 800)

grad.pars1 <- transform(base.pars, pc.increase = 3, nu_is = 3, behave.start = 25, T_c = 2)
syph_sim.grad1 <- lsoda(yini.co, func = gfun.change2(grad.pars1), parms = grad.pars1, times = tvec)
grad.pars2 <- transform(base.pars, pc.increase = 3, nu_is = 3, behave.start = 25, T_c = 5)
syph_sim.grad2 <- lsoda(yini.co, func = gfun.change2(grad.pars2), parms = grad.pars2, times = tvec)
grad.pars3 <- transform(base.pars, pc.increase = 3, nu_is = 3, behave.start = 25, T_c = 10)
syph_sim.grad3 <- lsoda(yini.co, func = gfun.change2(grad.pars3), parms = grad.pars3, times = tvec)


syphList3 <- list()
stateNames3 <- c("grad1","grad2", "grad3")

for(m in stateNames3){
	
	data <- get(paste0("syph_sim.",m))
	
	syphList3[[m]]<- data.frame(
		tvec,
		rowSums(data[,8:13])/rowSums(data[,2:19])
	)	
	names(syphList3[[m]]) <- c("time", "Prevalence")
}

sumList3 <- melt(syphList3, id.vars = "time")

sumList3$L1 <- factor(sumList3$L1, levels = c("grad3", "grad2", "grad1"))

p3 <- ggplot(sumList3, aes(time, value, col = L1)) + geom_line() +
	ggtitle("C") +
	theme(plot.title = element_text(size = 24, hjust = 0),
		legend.position=c(0.75,0.92),
		legend.text = element_text(size = 11)) +
	labs(x = "Time (years)", y = " ") + scale_y_continuous(limits = c(0,0.05)) +
	scale_color_manual(values=c("blue", "darkgreen", "black"),
		breaks = c("grad1", "grad2", "grad3"),
		labels = c("Characteristic time of 2 years", "Characteristic time of 5 years", "Characteristic time of 10 years"),
		name = "")

save("sumList1", "sumList2", "sumList3", file = "plot.rda")

plot <- grid.arrange(p1,p2, p3, nrow = 1)

ggsave(plot, file="finalPlot2.jpg", dpi = 800)

max(syphList[["behave0"]][,2]) ##0.032
 
max(syphList[["behave2"]][,2]) ##0.033

max(syphList2[["both1"]][,2]) ##0.108

max(syphList2[["both2"]][,2]) ##0.068

max(syphList[["ARVonly"]][-(1:500),2]) ##0.023

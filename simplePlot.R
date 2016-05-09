load("plotSimple.rda")
library("ggplot2"); theme_set(theme_classic())
library("gridExtra")
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

plot <- grid.arrange(p1,p2, nrow = 1)

ggsave(plot, file="finalPlot2.jpg", dpi = 800)
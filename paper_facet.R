
library("ggplot2"); theme_set(theme_classic())

baseFrame <- subset(baseFrame, L1==1 | time > behave.start)
suscFrame <- subset(suscFrame, L1==1 | time > behave.start)

baseFrame$Susc <- 1
suscFrame$Susc <- 2

facetFrame <- rbind(baseFrame, suscFrame)

facetFrame$Susc <- factor(facetFrame$Susc)
levels(facetFrame$Susc) <- c(
	"No biological effect", "Increased susceptibility"
)

fplot <- (
	ggplot(facetFrame , aes(time, value, col = scenario))
	+ geom_line()
	+ labs(x = "Time (years)", y = "Prevalence")
	+ scale_color_manual(name="Behavior change"
		, values=c("black", "red", "blue")
	)
	+ facet_wrap (~ Susc)
)

dev.off(); pdf(pdfname, height=5)
print(fplot)
# print(fplot + facet_wrap (~ Susc, nrow=2))


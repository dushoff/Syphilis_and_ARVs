library("ggplot2"); theme_set(theme_classic())

psname <- paste0(rtargetname, ".eps")
ggsave(psname, plot=fplot, dpi=1000)

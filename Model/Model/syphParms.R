n.trial <- 1000  ## total number of sims

Pars.skeleton <- list(
  c = c(40, 5),
  eps_b = 0.4,
  eps_a = 0.5,
  sigma = 0.015,
  tau = 1,
  beta1 = 0.627,
  beta2 = 0.618,
  D1 = 46/365,
  D2 = 108/365,
  p = 0.25,
  eps_R = 0.5, ##?
  iniI = 0.001,
  N0 = c(0.2,0.8)
)

##some sources...

##Steiner et al. Risk behavior for HIV transmission among gay men...
##Garnett et al. The Natural History of Syphilis...
##Granich et al. Universal voluntary HIV testing with...

Pars.range <- data.frame(min=c(1/60,0.1,0.05,1/16),
                            max=c(1/40,1,0.25,1/4),
                            row.names=c("mu","rho", "c_w","alpha_h"))
ltab <- as.data.frame(apply(Pars.range,1,
                            function(x) exp(seq(log(x[1]),log(x[2]),
                                                length=n.trial))))

set.seed(101)
ltab[] <- lapply(ltab,sample)

as.parlist <- function(x) {
  res <- append(x,Pars.skeleton)
  class(res) <- c("list","parlist")
  return(res)
}


geom_mean <- function(a){
	exp(mean(log(a)))
}

pars.mean <- as.parlist(apply(Pars.range, 1, geom_mean))

tvec <- seq(0,80,by=0.1)
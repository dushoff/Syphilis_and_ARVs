n.trial <- 1000  ## total number of sims

Pars.skeleton <- list(
  c <- c(0.08, 0.1),
  eps_b <- 0.9,
  eps_a <- 0.9,
  sigma <- 1/10,
  tau <- 1/5
)


Pars.range <- data.frame(min=c(1/60,0.5,0.05,1/16),
                            max=c(1/40,1,0.25,1/4),
                            row.names=c("mu","rho", "c_w","alpha"))
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

tvec <- seq(0,400,by=1)
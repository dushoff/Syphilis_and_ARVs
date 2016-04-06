n.trial <- 1000  ## total number of sims
set.seed(101)

## Non-randomized LHS data frame
ltab <- as.data.frame(apply(
	Pars.range, 1, function(x){
		exp(seq(log(x[1]),log(x[2]), length=n.trial))
	}
))

colnames(ltab) <- Pars.range[,1]

# Randomize and re-data-frame
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

expand <- function(x, ...) {
    UseMethod("expand")
}
expand.parlist <- function(x,...) {
    x <- within(x,
       {
          beta.HIV = c * c_w
          beta.syph = c * beta
       })
    return(x)
}

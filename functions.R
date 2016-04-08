
as.parlist <- function(x) {
	res <- append(x,Pars.skeleton)
	class(res) <- c("list","parlist")
	return(res)
}

geom_mean <- function(a){
	exp(mean(log(a)))
}

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

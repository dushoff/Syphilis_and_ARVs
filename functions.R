as.parlist <- function(x) {
	res <- x
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
          nuT_vec <- c(1, nu_t, 1)
          nuR_mat <- matrix(rep(c(1,nu_r,1),2),3,2)
          nuIS_mat <- matrix(rep(c(1,1,nu_is),2),3,2)
          nuHIV_vec <- c(1, nu_HIV, nu_ARV)
       })
    return(x)
}

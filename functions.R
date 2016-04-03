expand <- function(x, ...) {
    UseMethod("expand")
}
expand.parlist <- function(x,...) {
    x <- within(x,
       {
          beta.HIV = c * c_w
          beta.syph = c * (beta1 * D1 + beta2 * D2)/(D1 + D2)
       })
    return(x)
}

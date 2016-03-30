expand <- function(x, ...) {
    UseMethod("expand")
}
expand.parlist <- function(x,...) {
    x <- within(x,
       {
          beta_h = c * c_w
          beta_s = c * (beta1 * D1 + beta2 * D2)/(D1 + D2)
          gamma = 1/(D1 + D2)
       })
    return(x)
}

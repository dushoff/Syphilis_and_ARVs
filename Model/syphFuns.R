expand <- function(x, ...) {
    UseMethod("expand")
}
expand.parlist <- function(x,...) {
    x <- within(x,
       {
          beta <- c * c_w
       })
    return(x)
}

hill <- function(x,a,b,p) {
    a*b^p/(b^p+x^p)
}


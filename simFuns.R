transform.list <- function(.data,...) {
    e <- eval(substitute(list(...)), .data, parent.frame())
    tags <- names(e)
    inx <- match(tags, names(.data))
    matched <- !is.na(inx)
    if (any(matched)) {
        .data[inx[matched]] <- e[matched]
    }
    res <- if (!all(matched)) {
        c(.data, e[!matched])
    } else .data
    ## preserve original class
    class(res) <- class(.data)
    res
}
#' Compute the mean of a set of subspaces
#'
#' Function computes the mean of a set of subspaces
#'
#' @param x A list of matrices
#' @details Function computes the mean of a set of symmetrix, positive-definite
#' matrices. Several methods are implemented:
#' @return The mean of the matrices in \code{x}.
#' @export

sbsp.mean <- function(x){

    n <- length(x)
    M <- x[[1]]
    for (i in 2:n) {
        w <- 1 / i
        M <- sbsp.interpolate(M, x[[i]], w)
    }
    return(M)

}

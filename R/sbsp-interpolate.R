#' Interpolation between two subspaces
#'
#' Function smoothly interpolates between two subspaces \code{x} and \code{y}.
#' The interpolation is parametrized by \code{t}, where \code{t=0} returns \code{x},
#' \code{t=1} returns \code{y}, and \code{t=.5} returns the mean of \code{x} and \code{y}
#'
#' @param x,y Real matrices
#' @param t Interpolation parameter in \code{[0,infinity)}
#' @export

sbsp.interpolate <- function(x, y, t){

    I <- diag(nrow(x))
    K <- (I - x %*% solve(t(x) %*% x) %*% t(x)) %*%
        (y %*% solve(t(x) %*% y))
    fit <- svd(K)
    theta <- atan(fit$d)
    return(x %*% fit$v %*% diag2(cos(theta * t)) + fit$u %*% diag2(sin(theta * t)))
}

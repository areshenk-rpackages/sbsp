#' Projection onto the tangent space
#'
#' Function projects an element of the Grassmann manifold onto the tangent space at p
#'
#' @param x A real matrix
#' @param p The point on whose tangent space to project x
#' @return A real matrix.
#' @export
#' @importFrom pracma gramSchmidt

sbsp.logmap <- function(x, p = NULL){

    x <- gramSchmidt(x)$Q
    p <- gramSchmidt(p)$Q

    fit <- svd(t(x) %*% p)
    Y <- x %*% (fit$u %*% t(fit$v))
    fit <- svd((diag(nrow(x)) - p %*% t(p)) %*% Y)
    Q <- fit$u
    S <- diag2(asin(fit$d))
    return( fit$u %*% S %*% t(fit$v) )

    #K <- solve(t(x) %*% p) %*% (t(x) - t(x) %*% p %*% t(p))
    #fit <- svd(K)
    #return(fit$v %*% diag(atan(fit$d)) %*% t(fit$u))

}

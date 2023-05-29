#' Projection onto the tangent space
#'
#' Function projects an element of the Grassmann manifold onto the tangent space at p
#'
#' @param x A real matrix
#' @param p The point on whose tangent space to project x
#' @return A real matrix.
#' @export

sbsp.logmap <- function(x, p = NULL){

    K <- solve(t(x) %*% p) %*% (t(x) - t(x) %*% p %*% t(p))
    fit <- svd(K)
    return(fit$v %*% diag(atan(fit$d)) %*% t(fit$u))

}

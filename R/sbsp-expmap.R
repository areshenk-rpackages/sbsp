#' Projection from the tangent space
#'
#' Function projects a tangent vector onto the Grassmann manifold.
#'
#' @param x A real matrix (a tangent vector)
#' @param p The point from whose tangent space to project x
#' @return A real matrix.
#' @export

sbsp.expmap <- function(x, p = NULL){

    fit <- svd(x)
    K <- p %*% fit$v %*% diag2(cos(fit$d)) %*% t(fit$v) +
        fit$u %*% diag2(sin(fit$d)) %*% t(fit$v)
    return(qr.Q(qr(K)))

}

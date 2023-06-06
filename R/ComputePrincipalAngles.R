#' Principal angles between subspaces
#'
#' Function computes the principal angles between the subspaces
#' spanned by the columns of two matrices x and y
#' @param x,y Real matrices
#' @export

ComputePrincipalAngles <- function(x, y, return.dims = F){

    # Orthonormal bases for x and y
    x.U <- svd(x)$u
    y.U <- svd(y)$u

    # SVD for cosine method
    K <- crossprod(x.U,y.U)
    sigma <- svd(K)$d

    if (ncol(x) >= ncol(y))
        y <- y.U - x.U %*% K
    else
        y <- x.U - tcrossprod(y.U, K)

    # 4. Compute SVD for sine
    mask <- sigma^2 >= 0.5
    if (any(mask)) {
        vals <- svd(y)$d
        vals[vals > 1] <- 1
        vals[vals < -1] <- -1
        mu_arcsin <- asin(vals)
    }
    else
        mu_arcsin <- rep(0, length(sigma))

    # 5. Compute the principal angles
    # with reverse ordering of sigma because smallest sigma belongs to largest
    # angle theta
    sigma[sigma > 1] <- 1
    sigma[sigma < -1] <- -1
    mu_arcsin[mask] <- acos(sigma[mask])

    if(return.dims) {

    }

    return(mu_arcsin)
}


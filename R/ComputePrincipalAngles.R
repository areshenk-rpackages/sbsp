#' Principal angles between subspaces
#'
#' Function computes the principal angles between the subspaces
#' spanned by the columns of two matrices x and y
#' @param x,y Real matrices
#' @export

ComputePrincipalAngles <- function(x, y){

    n1 <- nrow(x)
    m1 <- ncol(x)
    n2 <- nrow(y)
    m2 <- ncol(y)
    if(p1 != p2){
        stop("Inputs must have equal numbers of rows")
    }

    x.U <- svd(x)$u
    y.U <- svd(y)$u
    pa <- acos(svd(crossprod(x.U,y.U))$d)

    return(pa)
}

#' Distance between two subspaces
#'
#' Function implements several distance measures between subspaces.
#'
#' @param x,y Real matrices
#' @param method The distance measure. See details.
#' @details Allowable distance measures are
#' \itemize{
#'  \item{"grassmann": }{The geodesic distance on the Grassmann manifold Gr(k,n).}
#'  \item{"grinf": }{A distance measure on the doubly infinite Grassmann manifold of subspaces of any dimension.}
#' }
#' @export

sbsp.dist <- function(x, y, method = 'grassmann', ...){

    # Wrapper
    d <- switch(method,
                grassmann = sbsp.dist.grassmann(x, y, ...),
                grinf = sbsp.dist.grinf(x, y, ...))
    return(d)
}

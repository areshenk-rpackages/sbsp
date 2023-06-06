#' @importFrom Matrix rankMatrix

sbsp.dist.grassmann <- function(x,y,...){
    k <- rankMatrix(x, method = 'qrLINPACK')[1]
    l <- rankMatrix(y, method = 'qrLINPACK')[1]

    if (k != l)
        stop('Grassmann distance is only defined for subspaces of the same dimension.')

    pa <- ComputePrincipalAngles(x,y)
    return(sqrt(sum(pa^2)))
}

sbsp.dist.grinf <- function(x,y,...){
    k <- rankMatrix(x, method = 'qrLINPACK')[1]
    l <- rankMatrix(y, method = 'qrLINPACK')[1]
    pa <- ComputePrincipalAngles(x,y)

    d <- sqrt( abs(k - l)*(pi^2/4) + sum(pa^2) )

    return(d)
}

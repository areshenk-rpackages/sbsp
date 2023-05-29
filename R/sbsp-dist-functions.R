sbsp.dist.grassmann <- function(x,y,...){
    pa <- ComputePrincipalAngles(x,y)
    return(sqrt(sum(pa^2)))
}

sbsp.dist.fs <- function(x,y,...){
    pa <- ComputePrincipalAngles(x,y)
    return(acos(prod(sin(pa))))
}

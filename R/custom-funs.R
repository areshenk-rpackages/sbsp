diag2 <- function(x) {
    if(length(x) > 1)
        return(diag(x))
    else
        return(matrix(x, nrow = 1, ncol = 1))
}

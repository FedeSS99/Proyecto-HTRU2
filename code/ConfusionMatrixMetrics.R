getmetrics_confmatrix <- function(M){
    accu <- (M[1, 1] + M[2, 2]) / sum(M)
    prec <- M[1,1] / sum(M[, 1])
    negpredvalue <- M[2, 2] / sum(M[, 2])
    sens <- M[1, 1] / sum(M[1, ])
    specifity <- M[2, 2] / sum(M[2, ])

    return(list(accu = accu, prec = prec, negpredvalue = negpredvalue, sens = sens, specifity = specifity))
}
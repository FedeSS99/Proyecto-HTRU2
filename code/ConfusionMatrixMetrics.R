getmetrics_confmatrix <- function(M){
    accu <- (M[1, 1] + M[2, 2]) / sum(M)

    sensivity <- M[1, 1] / sum(M[1, ])
    specifity <- M[2, 2] / sum(M[2, ])

    precision <- M[1,1] / sum(M[, 1])
    negpredvalue <- M[2, 2] / sum(M[, 2])


    return(list(accuracy = accu, sensivity = sensivity, specifity = specifity, precision = precision, negpredvalue = negpredvalue))
}

print_metrics <- function(metrics){
    for (metric in names(metrics)) {
        print(paste(metric, "=", metrics[[metric]], sep=""))
    }
}
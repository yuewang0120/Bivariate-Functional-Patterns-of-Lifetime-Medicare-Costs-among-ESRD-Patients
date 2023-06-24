library(dplyr)
library(parallel)
library(Rcpp)
# library(bigmemory)
sourceCpp("kfit.cpp")
sourceCpp("kres.cpp")
sourceCpp("sandwich.cpp")
kfit.p <- function(x, y, t, s, teval, seval, h, ncores = 1) {
    cl <- makeCluster(ncores)
    jobid <- sort(rep(1:ncores, length.out = length(teval)))
    clusterExport(cl, c('x', 'y', 't', 's', 'teval', 'seval', 'h', 'jobid'), environment())
    result <- parLapply(cl, 1:ncores, function(z) {
        Rcpp::sourceCpp('kfit.cpp')
        kfit(x, y, t, s, teval[jobid==z], seval[jobid==z], h)
    })
    stopCluster(cl)
    do.call(rbind, result)
}
kres.p <- function(x, y, t, s, pos, h, ncores = 1) {
    cl <- makeCluster(ncores)
    jobid <- sort(rep(1:ncores, length.out = length(pos)))
    clusterExport(cl, c('x', 'y', 't', 's', 'h', 'pos', 'jobid'), environment())
    result <- parLapply(cl, 1:ncores, function(z) {
        Rcpp::sourceCpp('kres.cpp')
        kres(x, y, t, s, pos[jobid==z], h)
    })
    stopCluster(cl)
    do.call(rbind, result)
}
# kfit_adaptive <- function(x, y, t, s, teval, seval, h) {
#     result <- kfit(x, y, t, s, teval, seval, h)
#     id <- is.na(result[,1])
#     while(sum(id)>0) {
#         h <- 2*h
#         result[id,] <- kfit(x, y, t, s, teval[id], seval[id], h)
#         id <- is.na(result[,1])
#     }
#     result
# }
kfit.pa <- function(x, y, t, s, teval, seval, h, ncores = 1) {
    result <- kfit.p(x, y, t, s, teval, seval, h, ncores)
    na <- is.na(result[, 1])
    while(sum(na) > 0) {
        h <- 2 * h
        result[na,] <- kfit.p(x, y, t, s, teval[na], seval[na], h, ncores)
        na <- is.na(result[, 1])
    }
    result
}
kres.pa <- function(x, y, t, s, pos, h, ncores = 1) {
    result <- kres.p(x, y, t, s, pos, h, ncores)
    na <- is.na(result[, 1])
    while(sum(na) > 0) {
        h <- 2 * h
        result[na,] <- kres.p(x, y, t, s, pos[na], h, ncores)
        na <- is.na(result[, 1])
    }
    result
}
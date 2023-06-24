source("functions2.R")
sim_n_persons <- function(n, m) {
    do.call(rbind, lapply(1:n, function(x) cbind(id = x, sim_one_person(m))))
}
beta1 <- function(t, s) {
    t / 4 * exp(-t^2 / 100 - s^2 / 100)
}
beta2 <- function(t, s) {
    0.5 * (sin(t * 0.4) - cos(s / 2))
}
sim_one_person <- function(m) {
    tau <- runif(1)
    tau <- c(tau, rbeta(m - 1, tau / 4 / 0.01^2, (1 - tau) / 4 / 0.01^2) + 1:(m - 1))
    x <- MASS::mvrnorm(mu = c(0, 0), Sigma = matrix(c(1, 0.5, 0.5, 1), 2))
    z1 <- as.numeric(x[1] > 0)
    z2 <- x[2]
    breaks <- cumsum(rexp(10, 1 / 20))
    x20 <- rbinom(1, 1, 0.5)
    x2 <- (x20 + rowSums(outer(tau, breaks, '>'))) %% 2
    death <- rexp(1, exp(0.5 * z1 + 0.2 * z2 - 3.5)) + 1
    censor <- runif(1, 20, 40)
    end <- min(death, censor)
    cor <- 0.5^abs(outer(tau, tau, "-"))
    sd <- diag(exp(0.5 - 0.05 * tau))
    u <- MASS::mvrnorm(mu = rep(0, nrow(cor)), Sigma = tcrossprod(sd) * cor)
    y <- beta1(tau, death - tau) + beta2(tau, death - tau) * x2 + z1 + z2 * 2 + u + rnorm(m)
    data.frame(tau = tau[tau <= end], z1 = z1, z2 = z2, x2 = x2[tau <= end], y = y[tau <= end], end = end, died = death < censor)
}
args <- commandArgs(trailingOnly = TRUE)
seed <- as.numeric(args[1])
h <- as.numeric(args[2])
fold <- as.numeric(args[3])
set.seed(seed)
data <- sim_n_persons(1000, 20) %>% filter(died)
uniq_id <- unique(data$id)
set.seed(123)
id_by_folds <- split(sample(uniq_id), rep(1:5, length.out = length(uniq_id)))
test <- data$id %in% id_by_folds[[fold]]
temp <- with(data[!test,], kres.pa(cbind(1, x2), cbind(y, z1, z2), tau, end - tau, 1:sum(!test), h, 19))
ztilde <- temp[,-1]
ytilde <- temp[,1]
fc1 <- c(solve(crossprod(ztilde, ztilde), crossprod(ztilde, ytilde)))
pres <- with(data[!test,], y - c(cbind(z1, z2) %*% fc1))
tvc1 <- with(data, kfit.pa(cbind(1, x2)[!test,], pres, tau[!test], (end - tau)[!test], tau[test], (end - tau)[test], h, 19))
res <- with(data[test,], y - c(cbind(z1, z2) %*% fc1) - rowSums(cbind(1, x2) * tvc1))
data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('simu_semi_cv_seed=', seed, '_h=', h, '_fold=', fold))

source("functions2.R")
sim_n_persons <- function(n, m) {
    do.call(rbind, lapply(1:n, function(x) cbind(id = x, sim_one_person(m))))
}
## Design 1
beta1 <- function(t, s) {
    t / 4 * exp(-t^2 / 100 - s^2 / 100)
}
beta2 <- function(t, s) {
    0.5 * (sin(t * 0.4) - cos(s / 2))
}
beta3 <- function(t, s) {
    cos((t^2 + s^2) / 100)
}
beta4 <- function(t, s) {
    exp(- t*s / (t^2 + s^2))
}
sim_one_person <- function(m) {
    tau <- runif(1)
    tau <- c(tau, rbeta(m - 1, tau / 4 / 0.01^2, (1 - tau) / 4 / 0.01^2) + 1:(m - 1))
    x <- MASS::mvrnorm(mu = c(0, 0), Sigma = matrix(c(1, 0.5, 0.5, 1), 2))
    x2 <- as.numeric(x[1] > 0)
    x3 <- x[2]
    death <- rexp(1, exp(0.5 * x2 + 0.2 * x3 - 3.5)) + 1
    censor <- runif(1, 20, 40)
    temp <- rbinom(1, 1, 0.3)
    temp2 <- rbinom(1, 1, 0.7)
    tspl <- (death<20)*(temp*runif(1, 0, death)+(1-temp)*10000) + (death>20)*(temp2*runif(1, 0, 20) + (1-temp2)*10000)
    end <- min(death, censor)
    cor <- 0.5^abs(outer(tau, tau, "-"))
    sd <- diag(exp(0.5 - 0.05 * tau))
    u <- MASS::mvrnorm(mu = rep(0, nrow(cor)), Sigma = tcrossprod(sd) * cor)
    y <- beta1(tau, death - tau) + beta2(tau-tspl, death-tau) * (tau>=tspl) + beta3(tau, death-tau) * x3 + beta4(tau, death-tau) * x2 + u + rnorm(m)
    data.frame(tau = tau[tau <= end], x2 = x2, x3 = x3, y = y[tau <= end], end = end, died = death < censor, tspl = ifelse(tspl < end, tspl, NA))
}
args <- commandArgs(trailingOnly = TRUE)
seed <- as.numeric(args[1])
h <- as.numeric(args[2])
fold <- as.numeric(args[3])
set.seed(seed)
data <- sim_n_persons(1000, 20) %>% filter(died, is.na(tspl))
uniq_id <- unique(data$id)
set.seed(123)
id_by_folds <- split(sample(uniq_id), rep(1:5, length.out = length(uniq_id)))
test <- data$id %in% id_by_folds[[fold]]
fit <- with(data, kfit.p(cbind(1, x2, x3)[!test,], y[!test], tau[!test], (end - tau)[!test], tau[test], (end - tau)[test], h, 19))
res <- with(data[test, ], y - rowSums(cbind(1, x2, x3) * fit))
dir.create('simu_mixed_cv1')
data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('simu_mixed_cv1/seed=', seed, '_h=', h, '_fold=', fold))

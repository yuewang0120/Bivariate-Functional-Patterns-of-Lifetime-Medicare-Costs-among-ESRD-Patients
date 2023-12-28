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
set.seed(seed)
data <- sim_n_persons(1000, 20) %>% filter(died)

## OLS fixed coef estimate
temp <- with(data, kres.pa(cbind(1, x2), cbind(y, z1, z2), tau, end - tau, 1:nrow(data), 1.12, 19))
ztilde <- temp[,-1]
ytilde <- temp[,1]
fc1 <- c(solve(crossprod(ztilde, ztilde), crossprod(ztilde, ytilde)))
res_fc1 <- ytilde - c(ztilde %*% fc1)
var_fc1 <- with(data, sandwich(ztilde, res_fc1, id, tau, end - tau, 0, 0, 1e10))

## OLS varying coef estimate
# grid <- rbind(cbind(seq(0, 5, 0.25), seq(5, 0, -0.25)), cbind(seq(0, 10, 0.5), seq(10, 0, -0.5)), cbind(seq(0, 15, 0.75), seq(15, 0, -0.75)))
grid <- expand.grid(seq(0, 20, 0.25), seq(0, 20, 0.25)) %>% filter(Var1 + Var2 <= 20)
tvc1 <- with(data, kfit.p(cbind(1, x2), y - c(cbind(z1, z2) %*% fc1), tau, end - tau, grid[,1], grid[,2], 1.12, 19))
var_tvc1 <- with(data, sandwich(cbind(1, x2), res_fc1, id, tau, end - tau, grid[,1], grid[,2], 1.12))

## WLS fixed coef estimate
sigma2 <- with(data, kfit.pa(matrix(1, nrow(data)), res_fc1^2, tau, end - tau, tau, end - tau, 1.12, 19)) %>% c()
fc2 <- c(solve(crossprod(ztilde, ztilde / sigma2), crossprod(ztilde, ytilde / sigma2)))
res_fc2 <- ytilde - c(ztilde %*% fc2)
temp <- lapply(unique(data$id), function(x) {
    temp2 <- data$id == x
    wi <- 1 / sigma2[temp2]
    t(ztilde[temp2,,drop=F] * wi) %*% (res_fc2[temp2] %o% res_fc2[temp2]) %*% (ztilde[temp2,,drop=F] * wi)
})
V <- Reduce('+', temp)
Dinv <- solve(crossprod(ztilde, ztilde / sigma2))
var_fc2 <- Dinv %*% V %*% Dinv
dir.create('simu_semi_fit')
list(fc1 = fc1, var_fc1 = var_fc1, tvc1 = tvc1, var_tvc1 = var_tvc1, fc2 = fc2, var_fc2 = var_fc2) %>% saveRDS(paste0('simu_semi_fit/seed=', seed, '_h=1.12'))

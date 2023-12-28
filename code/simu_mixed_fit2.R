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
pres <- readRDS(paste0('simu_mixed_fit12/h1=2.1_seed=', seed))
# pres <- readRDS(paste0('../../simu_mixed_fit12/h=2.1_seed=', seed))
set.seed(seed)
data <- sim_n_persons(1000, 20) %>% filter(died, !is.na(tspl), tau >= tspl)
grid <- expand.grid(seq(0, 20, 0.25), seq(0, 20, 0.25)) %>% filter(Var1 + Var2 <= 20)
fit <- with(data, kfit.p(matrix(1, nrow(data)), pres, tau - tspl, end - tau, grid[,1], grid[,2], 1.1, 19))
res <- with(data, kres.pa(matrix(1, nrow(data)), matrix(pres), tau - tspl, end - tau, 1:nrow(data), 1.1, 19))
var <- with(data, sandwich(matrix(1, nrow(data)), res, id, tau - tspl, end - tau, grid[,1], grid[,2], 1.1))
dir.create('simu_mixed_fit2')
list(est = fit, var = var) %>% saveRDS(paste0('simu_mixed_fit2/h1=2.1_h2=1.1_seed=', seed))

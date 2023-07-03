source("functions2.R")
data <- readRDS('../data/usrds1')
h <- 19
temp <- with(data, kres.pa(cbind(1, PD), cbind(log(claim / 100 + 1), waitlisted, waitlisted * time1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30), time1, time2, 1:nrow(data), h, 19))
ztilde <- temp[,-1]
ytilde <- temp[,1]
fc1 <- c(solve(crossprod(ztilde, ztilde), crossprod(ztilde, ytilde)))
res_fc1 <- ytilde - c(ztilde %*% fc1)

pres <- with(data, log(claim / 100 + 1) - c(cbind(waitlisted, waitlisted * time1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) %*% fc1))
grid <- cbind(c(seq(0, 500, 4), seq(0, 1250, 10), seq(0, 2000, 16)), c(seq(500, 0, -4), seq(1250, 0, -10), seq(2000, 0, -16)))
tvc1 <- with(data, kfit.p(cbind(1, PD), pres, time1, time2, grid[,1], grid[,2], h, 19))
var_tvc1 <- with(data, sandwich(cbind(1, PD), res_fc1, pseudo_id, time1, time2, grid[,1], grid[,2], h))

sigma2 <- with(data, kfit.pa(matrix(1, nrow(data)), res_fc1^2, time1, time2, time1, time2, h, 19)) %>% c()
fc2 <- c(solve(crossprod(ztilde, ztilde / sigma2), crossprod(ztilde, ytilde / sigma2)))
res_fc2 <- ytilde - c(ztilde %*% fc2)
temp <- lapply(unique(data$pseudo_id), function(x) {
    temp2 <- data$pseudo_id == x
    wi <- 1 / sigma2[temp2]
    t(ztilde[temp2,,drop=F] * wi) %*% (res_fc2[temp2] %o% res_fc2[temp2]) %*% (ztilde[temp2,,drop=F] * wi)
})
V <- Reduce('+', temp)
Dinv <- solve(crossprod(ztilde, ztilde / sigma2))
var_fc2 <- Dinv %*% V %*% Dinv
save(tvc1, var_tvc1, fc2, var_fc2, res_fc2, file = paste0('real_semi_fit'))

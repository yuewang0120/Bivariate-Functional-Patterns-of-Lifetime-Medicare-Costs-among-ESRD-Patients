source("functions2.R")
data <- readRDS('../data/usrds1')
uniq_id <- unique(data$pseudo_id)
set.seed(123)
id_by_folds <- split(sample(uniq_id), rep(1:5, length.out = length(uniq_id)))
args <- commandArgs(trailingOnly = TRUE)
h <- as.numeric(args[1])
fold <- as.numeric(args[2])
test <- data$pseudo_id %in% id_by_folds[[fold]]
temp <- with(data[!test,], kres.pa(cbind(1, PD), cbind(log(claim / 100 + 1), waitlisted, waitlisted * time1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30), time1, time2, 1:sum(!test), h, 19))
ztilde <- temp[,-1]
ytilde <- temp[,1]
fc1 <- c(solve(crossprod(ztilde, ztilde), crossprod(ztilde, ytilde)))
pres <- with(data[!test,], log(claim / 100 + 1) - c(cbind(waitlisted, waitlisted * time1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) %*% fc1))
tvc1 <- with(data, kfit.pa(cbind(1, PD)[!test,], pres, time1[!test], time2[!test], time1[test], time2[test], h, 19))
res <- with(data[test,], log(claim / 100 + 1) - c(cbind(waitlisted, waitlisted * time1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) %*% fc1) - rowSums(cbind(1, PD) * tvc1))
dir.create('real_semi_cv')
data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_semi_cv/h=', h, '_fold=', fold))

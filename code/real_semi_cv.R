source("functions2.R")
load('../data/usrds2.RData')
data2 <- data
load('../data/usrds1.RData')
data <- data %>% bind_rows(data2)
uniq_id <- unique(data$USRDS_ID)
set.seed(123)
id_by_folds <- split(sample(uniq_id), rep(1:5, length.out = length(uniq_id)))
args <- commandArgs(trailingOnly = TRUE)
h <- as.numeric(args[1])
fold <- as.numeric(args[2])
test <- data$USRDS_ID %in% id_by_folds[[fold]]
temp <- with(data[!test,], kres.pa(cbind(1, RXDETAIL > 4), cbind(log((ip+op+sn+hs+hh)/100+1), !is.na(CAN_FIRST_LISTING_DT), (!is.na(CAN_FIRST_LISTING_DT)) * (time - as.numeric(FIRST_SE)), RACE == 2, RACE > 2, SEX == 2, INC_AGE - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30), time - as.numeric(FIRST_SE), as.numeric(DIED) - time, 1:sum(!test), h, 19))
ztilde <- temp[,-1]
ytilde <- temp[,1]
fc1 <- c(solve(crossprod(ztilde, ztilde), crossprod(ztilde, ytilde)))
pres <- with(data[!test,], log((ip+op+sn+hs+hh)/100+1) - c(cbind(!is.na(CAN_FIRST_LISTING_DT), (!is.na(CAN_FIRST_LISTING_DT)) * (time - as.numeric(FIRST_SE)), RACE == 2, RACE > 2, SEX == 2, INC_AGE - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) %*% fc1))
tvc1 <- with(data, kfit.pa(cbind(1, RXDETAIL > 4)[!test,], pres, (time - as.numeric(FIRST_SE))[!test], (as.numeric(DIED) - time)[!test], (time - as.numeric(FIRST_SE))[test], (as.numeric(DIED) - time)[test], h, 19))
res <- with(data[test,], log((ip+op+sn+hs+hh)/100+1) - c(cbind(!is.na(CAN_FIRST_LISTING_DT), (!is.na(CAN_FIRST_LISTING_DT)) * (time - as.numeric(FIRST_SE)), RACE == 2, RACE > 2, SEX == 2, INC_AGE - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) %*% fc1) - rowSums(cbind(1, RXDETAIL > 4) * tvc1))
data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_semi_cv_h=', h, '_fold=', fold))

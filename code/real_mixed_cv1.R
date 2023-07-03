source("functions2.R")
data <- readRDS('../data/usrds2')
uniq_id <- unique(data$pseudo_id)
set.seed(123)
id_by_folds <- split(sample(uniq_id), rep(1:5, length.out = length(uniq_id)))
args <- commandArgs(trailingOnly = TRUE)
h <- as.numeric(args[1])
fold <- as.numeric(args[2])
type <- as.numeric(args[3])
test <- data$pseudo_id %in% id_by_folds[[fold]]
if (type == 1) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30)[!test,], log(ip / 100 + 1)[!test], time1[!test], time2[!test], time1[test], time2[test], h, 19))
    res <- with(data[test, ], log(ip / 100 + 1) - rowSums(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30) * fit))
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv1_ip_h=', h, '_fold=', fold))
} else if (type == 2) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30)[!test,], log(op / 100 + 1)[!test], time1[!test], time2[!test], time1[test], time2[test], h, 19))
    res <- with(data[test, ], log(op / 100 + 1) - rowSums(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30) * fit))
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv1_op_h=', h, '_fold=', fold))
} else if (type == 3) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30)[!test,], log(sn / 100 + 1)[!test], time1[!test], time2[!test], time1[test], time2[test], h, 19))
    res <- with(data[test, ], log(sn / 100 + 1) - rowSums(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30) * fit))
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv1_sn_h=', h, '_fold=', fold))
} else if (type == 4) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30)[!test,], log(hh / 100 + 1)[!test], time1[!test], time2[!test], time1[test], time2[test], h, 19))
    res <- with(data[test, ], log(hh / 100 + 1) - rowSums(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30) * fit))
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv1_hh_h=', h, '_fold=', fold))
} else if (type == 5) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30)[!test,], log(hs / 100 + 1)[!test], time1[!test], time2[!test], time1[test], time2[test], h, 19))
    res <- with(data[test, ], log(hs / 100 + 1) - rowSums(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30) * fit))
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv1_hs_h=', h, '_fold=', fold))
} else {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30)[!test,], log((ip + op + sn + hh + hs) / 100 + 1)[!test], time1[!test], time2[!test], time1[test], time2[test], h, 19))
    res <- with(data[test, ], log((ip + op + sn + hh + hs) / 100 + 1) - rowSums(cbind(1, race == 2, race > 2, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25, bmi >= 30) * fit))
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv1_all_h=', h, '_fold=', fold))
}

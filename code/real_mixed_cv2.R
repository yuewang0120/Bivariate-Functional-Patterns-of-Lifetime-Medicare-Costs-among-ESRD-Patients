source("functions2.R")
data <- readRDS('../data/usrds2')
group3 <- with(data, !is.na(time3) & time3 >= 0)
uniq_id <- unique(data$pseudo_id[group3])
set.seed(123)
id_by_folds <- split(sample(uniq_id), rep(1:5, length.out = length(uniq_id)))
args <- commandArgs(trailingOnly = TRUE)
h <- as.numeric(args[1])
fold <- as.numeric(args[2])
type <- as.numeric(args[3])
train <- data$pseudo_id[group3] %in% unlist(id_by_folds[-fold])
test <- data$pseudo_id[group3] %in% id_by_folds[[fold]]
if (type == 1) {
    pres <- c()
    for (i in 1:5) {
        temp <- readRDS(paste0('real_mixed_fit12_ip_part=', i))
        pres <- c(pres, temp)
    }
    fit <- with(data[group3,], kfit.pa(matrix(1, sum(train)), pres[train], time3[train], time2[train], time3[test], time2[test], h, 19))
    res <- pres[test] - c(fit)
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv2_ip_h=', h, '_fold=', fold))
} else if (type == 2) {
    pres <- c()
    for (i in 1:5) {
        temp <- readRDS(paste0('real_mixed_fit12_op_part=', i))
        pres <- c(pres, temp)
    }
    fit <- with(data[group3,], kfit.pa(matrix(1, sum(train)), pres[train], time3[train], time2[train], time3[test], time2[test], h, 19))
    res <- pres[test] - c(fit)
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv2_op_h=', h, '_fold=', fold))
} else if (type == 3) {
    pres <- c()
    for (i in 1:5) {
        temp <- readRDS(paste0('real_mixed_fit12_sn_part=', i))
        pres <- c(pres, temp)
    }
    fit <- with(data[group3,], kfit.pa(matrix(1, sum(train)), pres[train], time3[train], time2[train], time3[test], time2[test], h, 19))
    res <- pres[test] - c(fit)
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv2_sn_h=', h, '_fold=', fold))
} else if (type == 4) {
    pres <- c()
    for (i in 1:5) {
        temp <- readRDS(paste0('real_mixed_fit12_hh_part=', i))
        pres <- c(pres, temp)
    }
    fit <- with(data[group3,], kfit.pa(matrix(1, sum(train)), pres[train], time3[train], time2[train], time3[test], time2[test], h, 19))
    res <- pres[test] - c(fit)
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv2_hh_h=', h, '_fold=', fold))
} else if (type == 5) {
    pres <- c()
    for (i in 1:5) {
        temp <- readRDS(paste0('real_mixed_fit12_hs_part=', i))
        pres <- c(pres, temp)
    }
    fit <- with(data[group3,], kfit.pa(matrix(1, sum(train)), pres[train], time3[train], time2[train], time3[test], time2[test], h, 19))
    res <- pres[test] - c(fit)
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv2_hs_h=', h, '_fold=', fold))
} else {
    pres <- c()
    for (i in 1:5) {
        temp <- readRDS(paste0('real_mixed_fit12_all_part=', i))
        pres <- c(pres, temp)
    }
    fit <- with(data[group3,], kfit.pa(matrix(1, sum(train)), pres[train], time3[train], time2[train], time3[test], time2[test], h, 19))
    res <- pres[test] - c(fit)
    data.frame(h = h, fold = fold, sse = sum(res^2, na.rm = T), nobs = sum(!is.na(res))) %>% saveRDS(paste0('real_mixed_cv2_all_h=', h, '_fold=', fold))
}
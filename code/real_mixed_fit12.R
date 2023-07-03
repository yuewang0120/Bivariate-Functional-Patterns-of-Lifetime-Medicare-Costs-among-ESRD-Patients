source("functions2.R")
data <- readRDS('../data/usrds2')
h1 <- c(60, 1000, 200, 300, 200, 300)
# load('../../cohort2_10p.RData')
# data2 <- data %>% transmute(race = ifelse(RACE > 2, 3, RACE), sex = SEX, age = INC_AGE, hypertension, other_comorbid, bmi, log_sn = log(sn / 100 + 1), time1 = time - as.numeric(FIRST_SE), time2 = as.numeric(DIED) - time)
# load('../../cohort3_10p.RData')
# data <- data %>% transmute(race = ifelse(RACE > 2, 3, RACE), sex = SEX, age = INC_AGE, hypertension, other_comorbid, bmi, log_sn = log(sn / 100 + 1), time1 = time - as.numeric(FIRST_SE), time2 = as.numeric(DIED) - time, time3 = time - as.numeric(TX1DATE)) %>% bind_rows(data2)
group2 <- with(data, is.na(time3))
group3 <- with(data, !is.na(time3) & time3 >= 0)
rowid <- sort(rep(1:10, length.out = sum(group3)))
args <- commandArgs(trailingOnly = TRUE)
part <- as.numeric(args[1])
type <- as.numeric(args[2])
if (type == 1) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30)[group2,], log(ip / 100 + 1)[group2], time1[group2], time2[group2], time1[group3][rowid == part], time2[group3][rowid == part], h1[type], 19))
    with(data[group3,][rowid == part,], log(ip / 100 + 1) - rowSums(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) * fit)) %>% saveRDS(paste0('real_mixed_fit12_ip_part=', part))
} else if (type == 2) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30)[group2,], log(op / 100 + 1)[group2], time1[group2], time2[group2], time1[group3][rowid == part], time2[group3][rowid == part], h1[type], 19))
    with(data[group3,][rowid == part,], log(op / 100 + 1) - rowSums(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) * fit)) %>% saveRDS(paste0('real_mixed_fit12_op_part=', part))
} else if (type == 3) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30)[group2,], log(sn / 100 + 1)[group2], time1[group2], time2[group2], time1[group3][rowid == part], time2[group3][rowid == part], h1[type], 19))
    with(data[group3,][rowid == part,], log(sn / 100 + 1) - rowSums(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) * fit)) %>% saveRDS(paste0('real_mixed_fit12_sn_part=', part))
} else if (type == 4) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30)[group2,], log(hh / 100 + 1)[group2], time1[group2], time2[group2], time1[group3][rowid == part], time2[group3][rowid == part], h1[type], 19))
    with(data[group3,][rowid == part,], log(hh / 100 + 1) - rowSums(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) * fit)) %>% saveRDS(paste0('real_mixed_fit12_hh_part=', part))
} else if (type == 5) {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30)[group2,], log(hs / 100 + 1)[group2], time1[group2], time2[group2], time1[group3][rowid == part], time2[group3][rowid == part], h1[type], 19))
    with(data[group3,][rowid == part,], log(hs / 100 + 1) - rowSums(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) * fit)) %>% saveRDS(paste0('real_mixed_fit12_hs_part=', part))
} else {
    fit <- with(data, kfit.pa(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30)[group2,], log((ip + op + sn + hh + hs) / 100 + 1)[group2], time1[group2], time2[group2], time1[group3][rowid == part], time2[group3][rowid == part], h1[type], 19))
    with(data[group3,][rowid == part,], log((ip + op + sn + hh + hs) / 100 + 1) - rowSums(cbind(1, race == 2, race == 3, sex == 2, age - 65, hypertension, other_comorbid, bmi >= 25 & bmi < 30, bmi >= 30) * fit)) %>% saveRDS(paste0('real_mixed_fit12_all_part=', part))
}

source("functions2.R")
data <- readRDS('../data/usrds2')
alltype <- c('ip', 'op', 'sn', 'hh', 'hs', 'all')
h2 <- c(6.4, 68.5, 50.2, 45.6, 13.7, 9.1)
# load('../../cohort3_10p.RData')
# data <- data %>% transmute(pseudo_id = USRDS_ID, time2 = as.numeric(DIED) - time, time3 = time - as.numeric(TX1DATE))
group3 <- with(data, !is.na(time3) & time3 >= 0)
args <- commandArgs(trailingOnly = TRUE)
type <- as.numeric(args[1])
pres <- c()
for (i in 1:10) {
    temp <- readRDS(paste0('real_mixed_fit12/', alltype[type], '_part=', i))
    pres <- c(pres, temp)
}
res <- with(data[group3,], kres.p(matrix(1, sum(group3)), as.matrix(pres), time3, time2, 1:sum(group3), h2[type], 19))
grid <- expand.grid(seq(0, 3000, 100), seq(0, 3000, 100)) %>% filter(Var1 + Var2 < 3000)
teval <- grid$Var1
seval <- grid$Var2
coef <- with(data[group3,], kfit(matrix(1, sum(group3)), pres, time3, time2, teval, seval, h2[type]))
var <- with(data[group3,], sandwich(matrix(1, sum(group3)), res, pseudo_id, time3, time2, teval, seval, h2[type]))
dir.create('real_mixed_fit2')
list(coef = coef, var = var) %>% saveRDS(paste0('real_mixed_fit2/', alltype[type]))

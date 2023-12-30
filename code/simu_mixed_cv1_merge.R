suppressMessages(library(dplyr))
rec <- rep(NA, 10)
for (seed in 1:10) {
    result <- c()
    for (h in 1:5) {
        for (fold in 1:5) {
            temp <- readRDS(paste0('simu_mixed_cv1/seed=', seed, '_h=', h, '_fold=', fold))
            result <- rbind(result, temp)
        }
    }
    rec[seed] <- result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% with(h[which.min(sse)])
}
print('The first-stage bandwidth is:')
print(mean(rec))
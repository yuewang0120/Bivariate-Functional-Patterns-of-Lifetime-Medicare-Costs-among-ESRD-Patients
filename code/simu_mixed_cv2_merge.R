library(dplyr)
for (seed in 1:10) {
    result <- c()
    for (h in c(1:10)) {
        for (fold in 1:5) {
            temp <- readRDS(paste0('simu_mixed_cv2_seed=', seed, '_h=', h, '_fold=', fold))
            result <- rbind(result, temp)
        }
    }
    # result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% print()
    result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% with(h[which.min(sse)]) %>% print()
}
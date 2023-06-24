library(dplyr)
for (seed in 1:10) {
    result <- c()
    for (h in seq(0.5, 3, 0.5)) {
        for (fold in 1:5) {
            temp <- readRDS(paste0('simu_mixed_cv1_seed=', seed, '_h=', h, '_fold=', fold))
            result <- rbind(result, temp)
        }
    }
    # result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% print()
    result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% with(h[which.min(sse)]) %>% print()
}
# 2 1.5 1.5 1.5 1.5
# files <- list.files('simu_mixed_cv1')
# for (i in 1:10) {
#     result <- c()
#     for (file in files) {
#         if (startsWith(file, paste0('design=2_seed=', i, '_'))) {
#             temp <- readRDS(paste0('simu_mixed_cv1/', file))
#             result <- rbind(result, temp)
#         }
#     }
#     result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% with(h[which.min(sse)]) %>% print()
# }
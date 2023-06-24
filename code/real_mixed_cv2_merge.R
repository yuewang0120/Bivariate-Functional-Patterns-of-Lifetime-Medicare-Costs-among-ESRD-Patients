library(dplyr)
result2 <- c()
# for (h in seq(10, 100, 10)) {
#     for (fold in 1:5) {
#         temp <- readRDS(paste0('real_mixed_cv2/all_h=300_h2=', h, '_fold=', fold))
#         result <- rbind(result, temp)
#     }
# }
# result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs))
files <- list.files('real_mixed_cv2')
for (file in files) {
    if (startsWith(file,'hsintcp_h=200')) {
        load(paste0('real_mixed_cv2/', file))
        # temp <- readRDS(paste0('real_semi_cv/', file))
        result2 <- rbind(result2, result)
    }
}
result2 %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs))

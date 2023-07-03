library(dplyr)
files <- list.files()
for (type in c('ip', 'op', 'sn', 'hh', 'hs', 'all')) {
    result <- c()
    for (file in files) {
        if (startsWith(file, paste0('real_mixed_cv2_', type))) {
            temp <- readRDS(file)
            result <- rbind(result, temp)
        }
    }
    result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% with(h[which.min(sse)]) %>% print()
}
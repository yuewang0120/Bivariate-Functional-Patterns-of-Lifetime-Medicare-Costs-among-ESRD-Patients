library(dplyr)
result <- c()
files <- list.files()
for (file in files) {
    if (startsWith(file, 'real_semi_cv_h=')) {
        temp <- readRDS(file)
        result <- rbind(result, temp)
    }
}
result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% with(h[which.min(sse)]) %>% print()

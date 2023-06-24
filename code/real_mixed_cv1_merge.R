library(dplyr)
result <- c()
files <- list.files()
for (file in files) {
    if (startsWith(file, 'real_mixed_cv1')) {
        temp <- readRDS(file)
        result <- rbind(result, temp)
    }
}
result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs))

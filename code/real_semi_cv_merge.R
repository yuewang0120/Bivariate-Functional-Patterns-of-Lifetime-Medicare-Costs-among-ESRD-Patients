suppressMessages(library(dplyr))
result <- c()
files <- list.files('real_semi_cv', full.names = T)
for (file in files) {
    temp <- readRDS(file)
    result <- rbind(result, temp)
}
print('The undersmoothed bandwidth is:')
h <- result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% with(h[which.min(sse)])
print(h / 9880 ^ 0.05)
print('Note this bandwidth is obtained from the pseudo dataset and is not the same as what is used in the paper.')

suppressMessages(library(dplyr))
files <- list.files('real_mixed_cv1', full.names = T)
print('The first-stage bandwidths for inpatient, outpatient, skilled nursing, home health, hospice and total costs are:')
for (type in c('ip', 'op', 'sn', 'hh', 'hs', 'all')) {
    result <- c()
    for (file in files) {
        if (startsWith(file, paste0('real_mixed_cv1/', type))) {
            temp <- readRDS(file)
            result <- rbind(result, temp)
        }
    }
    result %>% group_by(h) %>% summarize(sse = sum(sse), nobs = sum(nobs)) %>% with(h[which.min(sse)]) %>% print()
}
print('Note these bandwidths are obtained from the pseudo dataset and are not the same as what are used in the paper.')

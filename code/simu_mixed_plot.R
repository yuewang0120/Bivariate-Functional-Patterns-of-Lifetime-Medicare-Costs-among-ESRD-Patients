grid <- rbind(cbind(seq(0, 5, 0.25), seq(5, 0, -0.25)), cbind(seq(0, 10, 0.5), seq(10, 0, -0.5)), cbind(seq(0, 15, 0.75), seq(15, 0, -0.75)))
est <- matrix(NA, nrow(grid), 100)
var <- matrix(NA, nrow(grid), 100)
for (i in 1:100) {
    temp <- readRDS(paste0('simu_mixed_fit2_h=1.1_seed=', i))
    est[,i] <- temp$est
    var[,i] <- temp$var
}
library(dplyr)
library(matrixStats)
library(ggplot2)
beta2 <- function(t, s) {
    0.5 * (sin(t * 0.4) - cos(s / 2))
}
true <- beta2(grid[,1], grid[,2])
png('simu_mixed_fit2_h=2.1_h2=1.1_est.png', width = 860, height = 300)
data.frame(t = grid[,1], s = grid[,2], true = true, est = est %>% rowMeans(), 
        #    upper = est %>% rowQuantiles(probs = 0.975), lower = est %>% rowQuantiles(probs = 0.025),
           upper2 = est %>% rowMeans() + 1.96 * sqrt(var) %>% rowMeans(), lower2 = est %>% rowMeans() - 1.96 * sqrt(var) %>% rowMeans(), 
           upper3 = est %>% rowMeans() + 1.96 * rowSds(est), lower3 = est %>% rowMeans() - 1.96 * rowSds(est), 
           beta = rep('gamma', each = nrow(grid)), death = rowSums(grid)) %>%
    mutate(period = t + s) %>%
    ggplot(aes(x = t)) + 
    geom_line(aes(y = true)) +
    geom_line(aes(y = est), linetype = 'dashed', color = 'red') + 
    geom_line(aes(y = upper2), linetype = 'dotted', color = 'blue') + 
    geom_line(aes(y = lower2), linetype = 'dotted', color = 'blue') + 
    geom_line(aes(y = upper3), linetype = 'dotdash', color = 'green') + 
    geom_line(aes(y = lower3), linetype = 'dotdash', color = 'green') + 
    facet_grid(beta ~ factor(paste0("'T-S=", period, "'"), levels=paste0("'T-S=", c(5, 10, 15), "'")), scale = 'free', labeller = label_parsed) +
    theme(text = element_text(size = 20), aspect.ratio = 1) + ylab('')
dev.off()
png('simu_mixed_fit2_h=1.02_h2=0.68_cover.png', width = 860, height = 250)
data.frame(t = grid[,1], s = grid[,2], coverage = rowMeans(est + 1.96 * sqrt(var) > true & est - 1.96 * sqrt(var) < true),
           beta = rep('gamma', each = nrow(grid)), death = rowSums(grid)) %>%
    mutate(period = t + s) %>%
    ggplot(aes(x = t)) + 
    geom_line(aes(y = 0.95), linetype = 'dotted') +
    geom_line(aes(y = coverage)) + 
    facet_grid(beta ~ factor(paste0("'T-S=", period, "'"), levels=paste0("'T-S=", c(5, 10, 15), "'")), scale = 'free_x', labeller = label_parsed) +
    theme(text = element_text(size = 20), aspect.ratio = 1) + ylab('') + ylim(c(0,1))
dev.off()
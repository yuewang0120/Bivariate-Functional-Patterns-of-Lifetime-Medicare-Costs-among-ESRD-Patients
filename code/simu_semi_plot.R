fc1 <- matrix(NA, 2, 500)
var_fc1 <- matrix(NA, 2, 500)
fc2 <- matrix(NA, 2, 500)
var_fc2 <- matrix(NA, 2, 500)
grid <- rbind(cbind(seq(0, 5, 0.25), seq(5, 0, -0.25)), cbind(seq(0, 10, 0.5), seq(10, 0, -0.5)), cbind(seq(0, 15, 0.75), seq(15, 0, -0.75)))
tvc1 <- matrix(NA, nrow(grid) * 2, 500)
var_tvc1 <- matrix(NA, nrow(grid) * 2, 500)
for (i in 1:500) {
    temp <- readRDS(paste0('simu_semi_fit_seed=', i, '_h=1.12'))
    fc1[,i] <- temp$fc1
    var_fc1[,i] <- temp$var_fc1[c(1,3)]
    tvc1[,i] <- temp$tvc1
    var_tvc1[,i] <- temp$var_tvc1[,c(1,3)]
    fc2[,i] <- temp$fc2
    var_fc2[,i] <- temp$var_fc2[c(1,4)]
}
## fixed coef
print('MSE of alpha hat with weight I:')
rowMeans((fc1 - 1:2)^2)
print('MSE of alpha hat with weight D:')
rowMeans((fc2 - 1:2)^2)
print('Ratio of the two MSEs:')
rowMeans((fc2 - 1:2)^2) / rowMeans((fc1 - 1:2)^2)
print('Coverage of alpha hat with weight I:')
rowMeans(fc1 + 1.96 * sqrt(var_fc1) > 1:2 & fc1 - 1.96 * sqrt(var_fc1) < 1:2)
print('Coverage of alpha hat with weight D:')
rowMeans(fc2 + 1.96 * sqrt(var_fc2) > 1:2 & fc2 - 1.96 * sqrt(var_fc2) < 1:2)

## varying coef
library(dplyr)
library(matrixStats)
library(ggplot2)
beta1 <- function(t, s) {
    t / 4 * exp(-t^2 / 100 - s^2 / 100)
}
beta2 <- function(t, s) {
    0.5 * (sin(t * 0.4) - cos(s / 2))
}
true <- c(beta1(grid[,1], grid[,2]), beta2(grid[,1], grid[,2]))
png('figure1.png', width = 860, height = 500)
data.frame(t = rep(grid[,1], 2), s = rep(grid[,2], 2), true = true, est = tvc1 %>% rowMeans(), 
           upper2 = tvc1 %>% rowMeans() + 1.96 * sqrt(var_tvc1) %>% rowMeans(), lower2 = tvc1 %>% rowMeans() - 1.96 * sqrt(var_tvc1) %>% rowMeans(), 
           upper3 = tvc1 %>% rowMeans() + 1.96 * rowSds(tvc1), lower3 = tvc1 %>% rowMeans() - 1.96 * rowSds(tvc1), 
           beta = rep(paste0('beta[', 1:2, ']'), each = nrow(grid)), death = rep(rowSums(grid), 2)) %>%
    mutate(period = t + s) %>%
    ggplot(aes(x = t)) + 
    geom_line(aes(y = true)) +
    geom_line(aes(y = est), linetype = 'dashed', color = 'red') + 
    geom_line(aes(y = upper3), linetype = 'dotted', color = 'blue') + 
    geom_line(aes(y = lower3), linetype = 'dotted', color = 'blue') + 
    geom_line(aes(y = upper2), linetype = 'dotdash', color = 'green') + 
    geom_line(aes(y = lower2), linetype = 'dotdash', color = 'green') + 
    facet_grid(beta ~ factor(paste0("'T=", period, "'"), levels=paste0("'T=", c(5, 10, 15), "'")), scale = 'free', labeller = label_parsed) +
    theme(text = element_text(size = 20), aspect.ratio = 1) + ylab('')
dev.off()

load('real_semi_fit')
print('Fixed coefficients, confidence intervals and P-values:')
library(dplyr)
cbind(fc2, fc2 - 1.96 * sqrt(diag(var_fc2)), fc2 + 1.96 * sqrt(diag(var_fc2)), 2*pnorm(-abs(fc2), sd = sqrt(diag(var_fc2)))) %>% signif(4) %>% '*'(1)


png('figure3.png', height=500, width=860)
teval <- c(seq(0, 500, 4), seq(0, 1250, 10), seq(0, 2000, 16))
seval <- c(seq(500, 0, -4), seq(1250, 0, -10), seq(2000, 0, -16))
library(ggplot2)
data.frame(x = rep(teval, 2), est = c(tvc1), 
           upper = c(tvc1 + 1.96*sqrt(var_tvc1[, c(1,3)])),
           lower = c(tvc1 - 1.96*sqrt(var_tvc1[, c(1,3)])),
           death = rep(teval + seval, 2), beta = rep(paste0('beta[', 1:2, ']'), each = length(teval))) %>%
    ggplot(aes(x = x)) + 
    geom_line(aes(y = est)) + 
    geom_line(aes(y = upper), linetype='dashed', color = 'red') + 
    geom_line(aes(y = lower), linetype='dashed', color = 'red') + 
    geom_line(aes(y = 0), linetype='dotted') + 
    facet_grid(beta~factor(paste0("'T=", death, "'"), levels=paste0("'T=", c(500, 1250, 2000), "'")), scales = 'free', space = 'free_x', labeller = label_parsed) +
    ylab("") +
    xlab("days since entry") + scale_x_continuous(n.breaks = 4) +
    theme(text = element_text(size = 20))
dev.off()

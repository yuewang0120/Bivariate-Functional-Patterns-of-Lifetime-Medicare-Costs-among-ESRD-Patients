teval <- c(seq(0, 800, 8), seq(0, 1600, 16), seq(0, 2200, 22))
seval <- c(seq(800, 0, -8), seq(1600, 0, -16), seq(2200, 0, -22))
temp <- readRDS('real_mixed_fit2_all')
coef <- temp$coef
var <- temp$var
library(ggplot2)
library(dplyr)
png('figure4.png', height=250, width=860)
data.frame(x = teval, est = c(coef), upper = c(coef + 1.96*sqrt(var)), lower = c(coef - 1.96*sqrt(var)), death = rep(teval + seval, 2)) %>%
    ggplot(aes(x = x)) + 
    geom_line(aes(y = est)) + 
    geom_line(aes(y = upper), linetype='dashed', color = 'red') + 
    geom_line(aes(y = lower), linetype='dashed', color = 'red') + 
    geom_line(aes(y = 0), linetype='dotted') + 
    facet_grid(~factor(paste0("'T-S=", death, "'"), levels=paste0("'T-S=", c(800, 1600, 2200), "'")), scales = 'free', space = 'free', labeller = label_parsed) +
    ylab("") +
    xlab("days since entry") +
    theme(text = element_text(size = 20))
dev.off()

temp <- readRDS('real_mixed_fit2_ip')
coef <- temp$coef
var <- temp$var
temp <- readRDS('real_mixed_fit2_op')
coef <- c(coef, temp$coef)
var <- c(var, temp$var)
temp <- readRDS('real_mixed_fit2_sn')
coef <- c(coef, temp$coef)
var <- c(var, temp$var)
temp <- readRDS('real_mixed_fit2_hh')
coef <- c(coef, temp$coef)
var <- c(var, temp$var)
temp <- readRDS('real_mixed_fit2_hs')
coef <- c(coef, temp$coef)
var <- c(var, temp$var)
png('figure5.png', height=1000, width=860)
temp <- c('IP', 'OP', 'SN', 'HH', 'HS')
data.frame(x = rep(teval, 5), est = coef, upper = coef + 1.96*sqrt(var), lower = coef - 1.96*sqrt(var), 
           death = rep(teval + seval, 5), type = rep(factor(temp, levels = temp), each = length(teval))) %>%
    ggplot(aes(x = x)) + 
    geom_line(aes(y = est)) + 
    geom_line(aes(y = upper), linetype='dashed', color = 'red') + 
    geom_line(aes(y = lower), linetype='dashed', color = 'red') + 
    geom_line(aes(y = 0), linetype='dotted') + 
    facet_grid(type~factor(paste0("'T-S=", death, "'"), levels=paste0("'T-S=", c(800, 1600, 2200), "'")), scales = 'free', space = 'free_x', labeller = label_parsed) +
    ylab("") +
    xlab("days since entry") +
    theme(text = element_text(size = 20))
dev.off()
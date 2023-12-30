# grid <- rbind(cbind(seq(0, 5, 0.25), seq(5, 0, -0.25)), cbind(seq(0, 10, 0.5), seq(10, 0, -0.5)), cbind(seq(0, 15, 0.75), seq(15, 0, -0.75)))
suppressMessages({library(dplyr)
library(matrixStats)
library(ggplot2)
library(plotly)})
grid <- expand.grid(seq(0, 20, 0.25), seq(0, 20, 0.25)) %>% filter(Var1 + Var2 <= 20)
subset <- with(grid, (Var1 + Var2) %in% c(5, 10, 15))
est <- matrix(NA, nrow(grid), 500)
var <- matrix(NA, nrow(grid), 500)
for (i in 1:500) {
    temp <- readRDS(paste0('simu_mixed_fit2/h1=2.1_h2=1.1_seed=', i))
    est[,i] <- temp$est
    var[,i] <- temp$var
}
beta2 <- function(t, s) {
    0.5 * (sin(t * 0.4) - cos(s / 2))
}
true <- beta2(grid[,1], grid[,2])
png('figure2.png', width = 860, height = 300)
data.frame(t = grid[,1], s = grid[,2], true = true, est = est %>% rowMeans(), 
           upper2 = est %>% rowMeans() + 1.96 * sqrt(var) %>% rowMeans(), lower2 = est %>% rowMeans() - 1.96 * sqrt(var) %>% rowMeans(), 
           upper3 = est %>% rowMeans() + 1.96 * rowSds(est), lower3 = est %>% rowMeans() - 1.96 * rowSds(est), 
           beta = rep('gamma', each = nrow(grid)), death = rowSums(grid)) %>%
    filter(subset) %>%
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

## 3d plot
# x <- sort(unique(grid[,1]))
# y <- sort(unique(grid[,2]))
# colid <- match(grid[,1], x)
# rowid <- match(grid[,2], y)
# id <- rowid + (colid - 1) * length(y)
# z1 <- matrix(NA, length(y), length(x))
# z1[id] <- true
# z2 <- matrix(NA, length(y), length(x))
# z2[id] <- rowMeans(est)
# z3 <- matrix(NA, length(y), length(x))
# z3[id] <- rowMeans(est) + rowSds(est) * 1.96
# z4 <- matrix(NA, length(y), length(x))
# z4[id] <- rowMeans(est) - rowSds(est) * 1.96
# z5 <- matrix(NA, length(y), length(x))
# z5[id] <- rowMeans(est) + 1.96 * rowMeans(sqrt(var))
# z6 <- matrix(NA, length(y), length(x))
# z6[id] <- rowMeans(est) - 1.96 * rowMeans(sqrt(var))
# plot_ly(x = x, y = y, showscale = F, showlegend = T) %>% 
# add_surface(z = z1, colorscale = list(c(0, 1), c("black", "black")), name = 'True values', opacity = 0.5) %>%
# add_surface(z = z2, colorscale = list(c(0, 1), c("red", "red")), name = 'Mean estimate', opacity = 0.5) %>%
# add_surface(z = z3, colorscale = list(c(0, 1), c("blue", "blue")), name = '95% Upper Empirical Band', opacity = 0.5) %>%
# add_surface(z = z4, colorscale = list(c(0, 1), c("blue", "blue")), name = '95% Lower Empirical Band', opacity = 0.5) %>%
# add_surface(z = z5, colorscale = list(c(0, 1), c("green", "green")), name = 'Mean of 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z6, colorscale = list(c(0, 1), c("green", "green")), name = 'Mean of 95% Lower Confidence Band', opacity = 0.5) %>%
# layout(title = '3D Plot of Figure 2') %>%
# htmlwidgets::saveWidget("figure2_3d.html")

# Wireframe
df <- data.frame(grid, true, rowMeans(est), 
                 rowMeans(est) + 1.96 * rowSds(est),
                 rowMeans(est) - 1.96 * rowSds(est),
                 rowMeans(est) + 1.96 * rowMeans(sqrt(var)),
                 rowMeans(est) - 1.96 * rowMeans(sqrt(var)))
names(df) <- c('x', 'y', 'true', 'est', 'emp upper', 'emp lower', 'ci upper', 'ci lower')

df1 <- split(df, df$y)
df2 <- split(df, df$x)
add_mesh <- function(p, legendname, legendgroup, color, columnname, showlegend) {
    for(i in seq_along(df1)){
        df_sp <- df1[[i]]
        p <- add_trace(p, 
            line = list(
                color = color, 
                width = 2
            ), 
            mode = "lines", 
            type = "scatter3d", 
            x = df_sp$x,
            y = df_sp$y,
            z = df_sp[[columnname]],
            showlegend = ifelse(i==1, showlegend, FALSE),
            name = legendname, 
            legendgroup = legendgroup
        )
    }
    for(i in seq_along(df2)){
        df_sp <- df2[[i]]
        p <- add_trace(p, 
            line = list(
                color = color, 
                width = 2
            ),
            mode = "lines", 
            type = "scatter3d", 
            x = df_sp$x,
            y = df_sp$y,
            z = df_sp[[columnname]],
            showlegend = FALSE,
            name = legendname, 
            legendgroup = legendgroup
        )
    }
    p
}
p1 <- add_mesh(plot_ly(), 'True values', 'group1', 'black', 'true', TRUE)
p1 <- add_mesh(p1, 'Mean estimate', 'group2', 'red', 'est', TRUE)
p1 <- add_mesh(p1, '95% Upper Empirical Band', 'group3', 'blue', 'emp upper', TRUE)
p1 <- add_mesh(p1, 'Mean of 95% Upper Confidence Band', 'group4', 'green', 'ci upper', TRUE)
p1 <- add_mesh(p1, '95% Lower Empirical Band', 'group5', 'blue', 'emp lower', TRUE)
p1 <- add_mesh(p1, 'Mean of 95% Lower Confidence Band', 'group6', 'green', 'ci lower', TRUE)
p1 %>% 
layout(title = "3D Plot of Figure 2", 
    scene = list(domain=list(x=c(0,1), y=c(0,1)),
        xaxis = list(range = c(0, 20)),
        yaxis = list(range = c(0, 20)),
        zaxis = list(range = c(-1.5, 1.5)),
        aspectmode='cube')) %>% 
layout(annotations = list(
    list(x = 0.5 , y = 0.95, text = "gamma", showarrow = F, xref='paper', yref='paper'))
) %>%
htmlwidgets::saveWidget("figure2_3d.html")

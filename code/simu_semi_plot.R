library(dplyr)
library(matrixStats)
library(ggplot2)
library(rgl)
library(plotly)
fc1 <- matrix(NA, 2, 500)
var_fc1 <- matrix(NA, 2, 500)
fc2 <- matrix(NA, 2, 500)
var_fc2 <- matrix(NA, 2, 500)
grid <- expand.grid(seq(0, 20, 0.25), seq(0, 20, 0.25)) %>% filter(Var1 + Var2 <= 20)
tvc1 <- matrix(NA, nrow(grid) * 2, 500)
var_tvc1 <- matrix(NA, nrow(grid) * 2, 500)
for (i in 1:500) {
    # temp <- readRDS(paste0('../../simu_semi_fit/design=2_n=1000_seed=', i, '_h=1.12'))
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
beta1 <- function(t, s) {
    t / 4 * exp(-t^2 / 100 - s^2 / 100)
}
beta2 <- function(t, s) {
    0.5 * (sin(t * 0.4) - cos(s / 2))
}
true <- c(beta1(grid[,1], grid[,2]), beta2(grid[,1], grid[,2]))
# png('figure1.png', width = 860, height = 500)
# data.frame(t = rep(grid[,1], 2), s = rep(grid[,2], 2), true = true, est = tvc1 %>% rowMeans(), 
#            upper2 = tvc1 %>% rowMeans() + 1.96 * sqrt(var_tvc1) %>% rowMeans(), lower2 = tvc1 %>% rowMeans() - 1.96 * sqrt(var_tvc1) %>% rowMeans(), 
#            upper3 = tvc1 %>% rowMeans() + 1.96 * rowSds(tvc1), lower3 = tvc1 %>% rowMeans() - 1.96 * rowSds(tvc1), 
#            beta = rep(paste0('beta[', 1:2, ']'), each = nrow(grid)), death = rep(rowSums(grid), 2)) %>%
#     filter((t + s) %in% c(5, 10, 15)) %>%
#     ggplot(aes(x = t)) + 
#     geom_line(aes(y = true)) +
#     geom_line(aes(y = est), linetype = 'dashed', alpha = 0.25, size = 1) + 
#     geom_point(aes(y = est)) + 
#     geom_line(aes(y = upper3), linetype = 'dotted') + 
#     geom_line(aes(y = lower3), linetype = 'dotted') + 
#     geom_line(aes(y = upper2), linetype = 'dashed', alpha = 0.25, size = 1) + 
#     geom_line(aes(y = lower2), linetype = 'dashed', alpha = 0.25, size = 1) + 
#     facet_grid(beta ~ factor(paste0("'T=", t+s, "'"), levels=paste0("'T=", c(5, 10, 15), "'")), scale = 'free', labeller = label_parsed) +
#     theme(text = element_text(size = 20), aspect.ratio = 1) + ylab('')
# dev.off()

png('figure1.png', width = 860, height = 500)
data.frame(t = rep(grid[,1], 2), s = rep(grid[,2], 2), true = true, est = tvc1 %>% rowMeans(), 
           upper2 = tvc1 %>% rowMeans() + 1.96 * sqrt(var_tvc1) %>% rowMeans(), lower2 = tvc1 %>% rowMeans() - 1.96 * sqrt(var_tvc1) %>% rowMeans(), 
           upper3 = tvc1 %>% rowMeans() + 1.96 * rowSds(tvc1), lower3 = tvc1 %>% rowMeans() - 1.96 * rowSds(tvc1), 
           beta = rep(paste0('beta[', 1:2, ']'), each = nrow(grid)), death = rep(rowSums(grid), 2)) %>%
    filter((t + s) %in% c(5, 10, 15)) %>%
    ggplot(aes(x = t)) + 
    geom_line(aes(y = true)) +
    geom_line(aes(y = est), linetype = 'dashed', color = 'red') + 
    geom_line(aes(y = upper3), linetype = 'dotted', color = 'blue') + 
    geom_line(aes(y = lower3), linetype = 'dotted', color = 'blue') + 
    geom_line(aes(y = upper2), linetype = 'dotdash', color = 'green') + 
    geom_line(aes(y = lower2), linetype = 'dotdash', color = 'green') + 
    facet_grid(beta ~ factor(paste0("'T=", t+s, "'"), levels=paste0("'T=", c(5, 10, 15), "'")), scale = 'free', labeller = label_parsed) +
    theme(text = element_text(size = 20), aspect.ratio = 1) + ylab('')
dev.off()


# png('temp.png', width = 860, height = 500)
# data.frame(t = rep(grid[,1], 2), s = rep(grid[,2], 2), true = true, est = tvc1 %>% rowMeans(), 
#            upper2 = tvc1 %>% rowMeans() + 1.96 * sqrt(var_tvc1) %>% rowMeans(), lower2 = tvc1 %>% rowMeans() - 1.96 * sqrt(var_tvc1) %>% rowMeans(), 
#            upper3 = tvc1 %>% rowMeans() + 1.96 * rowSds(tvc1), lower3 = tvc1 %>% rowMeans() - 1.96 * rowSds(tvc1), 
#            beta = rep(paste0('beta[', 1:2, ']'), each = nrow(grid)), death = rep(rowSums(grid), 2)) %>%
#     filter((t + s) %in% c(5, 10, 15)) %>%
#     ggplot(aes(x = t)) + 
#     geom_line(aes(y = true), alpha = 0.25) +
#     geom_point(aes(y = true), shape = 1) +
#     geom_line(aes(y = est), alpha = 0.25) + 
#     geom_point(aes(y = est), shape = 0) + 
#     geom_line(aes(y = upper3), alpha = 0.25) + 
#     geom_point(aes(y = upper3), shape = 3) + 
#     geom_line(aes(y = lower3), alpha = 0.25) + 
#     geom_point(aes(y = lower3), shape = 3) + 
#     geom_line(aes(y = upper2), alpha = 0.25) + 
#     geom_point(aes(y = upper2), shape = 4) + 
#     geom_line(aes(y = lower2), alpha = 0.25) + 
#     geom_point(aes(y = lower2), shape = 4) + 
#     facet_grid(beta ~ factor(paste0("'T=", t+s, "'"), levels=paste0("'T=", c(5, 10, 15), "'")), scale = 'free', labeller = label_parsed) +
#     theme(text = element_text(size = 20), aspect.ratio = 1) + ylab('')
# dev.off()


## 3d plot
# x <- sort(unique(grid[,1]))
# y <- sort(unique(grid[,2]))
# colid <- match(grid[,1], x)
# rowid <- match(grid[,2], y)
# id <- rowid + (colid - 1) * length(y)
# z1 <- matrix(NA, length(y), length(x))
# z1[id] <- (true %>% matrix(ncol = 2))[,1]
# z2 <- matrix(NA, length(y), length(x))
# z2[id] <- (rowMeans(tvc1) %>% matrix(ncol = 2))[,1]
# z3 <- matrix(NA, length(y), length(x))
# z3[id] <- matrix(rowMeans(tvc1) + rowSds(tvc1) * 1.96, ncol = 2)[,1]
# z4 <- matrix(NA, length(y), length(x))
# z4[id] <- matrix(rowMeans(tvc1) - rowSds(tvc1) * 1.96, ncol = 2)[,1]
# z5 <- matrix(NA, length(y), length(x))
# z5[id] <- matrix(rowMeans(tvc1) + 1.96 * rowMeans(sqrt(var_tvc1)), ncol = 2)[,1]
# z6 <- matrix(NA, length(y), length(x))
# z6[id] <- matrix(rowMeans(tvc1) - 1.96 * rowMeans(sqrt(var_tvc1)), ncol = 2)[,1]
# z7 <- matrix(NA, length(y), length(x))
# z7[id] <- (true %>% matrix(ncol = 2))[,2]
# z8 <- matrix(NA, length(y), length(x))
# z8[id] <- (rowMeans(tvc1) %>% matrix(ncol = 2))[,2]
# z9 <- matrix(NA, length(y), length(x))
# z9[id] <- matrix(rowMeans(tvc1) + rowSds(tvc1) * 1.96, ncol = 2)[,2]
# z10 <- matrix(NA, length(y), length(x))
# z10[id] <- matrix(rowMeans(tvc1) - rowSds(tvc1) * 1.96, ncol = 2)[,2]
# z11 <- matrix(NA, length(y), length(x))
# z11[id] <- matrix(rowMeans(tvc1) + 1.96 * rowMeans(sqrt(var_tvc1)), ncol = 2)[,2]
# z12 <- matrix(NA, length(y), length(x))
# z12[id] <- matrix(rowMeans(tvc1) - 1.96 * rowMeans(sqrt(var_tvc1)), ncol = 2)[,2]


# persp(x, y, z1, theta = 30, phi = 30, col = "lightblue", border = "gray")
# png('Rplots.pdf')
# rgl.surface(x, y, z1, color = "red")
# rgl.surface(x, y, z2, color = "blue", add = TRUE)
# rglwidget()
# dev.off()
# p1 <- plot_ly(x = x, y = y, showscale = F, showlegend = T, scene = 'scene1') %>% 
# add_surface(z = z1, colorscale = list(c(0, 1), c("black", "black")), name = 'True values', opacity = 0.5) %>%
# add_surface(z = z2, colorscale = list(c(0, 1), c("red", "red")), name = 'Mean estimate', opacity = 0.5) %>%
# add_surface(z = z3, colorscale = list(c(0, 1), c("blue", "blue")), name = '95% Upper Empirical Band', opacity = 0.5) %>%
# add_surface(z = z4, colorscale = list(c(0, 1), c("blue", "blue")), name = '95% Lower Empirical Band', opacity = 0.5) %>%
# add_surface(z = z5, colorscale = list(c(0, 1), c("green", "green")), name = 'Mean of 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z6, colorscale = list(c(0, 1), c("green", "green")), name = 'Mean of 95% Lower Confidence Band', opacity = 0.5)
# p2 <- plot_ly(x = x, y = y, showscale = F, showlegend = T, scene = 'scene2')%>% 
# add_surface(z = z7, colorscale = list(c(0, 1), c("black", "black")), name = 'True values', opacity = 0.5) %>%
# add_surface(z = z8, colorscale = list(c(0, 1), c("red", "red")), name = 'Mean estimate', opacity = 0.5) %>%
# add_surface(z = z9, colorscale = list(c(0, 1), c("blue", "blue")), name = '95% Upper Empirical Band', opacity = 0.5) %>%
# add_surface(z = z10, colorscale = list(c(0, 1), c("blue", "blue")), name = '95% Lower Empirical Band', opacity = 0.5) %>%
# add_surface(z = z11, colorscale = list(c(0, 1), c("green", "green")), name = 'Mean of 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z12, colorscale = list(c(0, 1), c("green", "green")), name = 'Mean of 95% Lower Confidence Band', opacity = 0.5)
# axx <- list(

#   gridcolor='rgb(255, 255, 255)',

#   zerolinecolor='rgb(255, 255, 255)',

#   showbackground=TRUE,

#   backgroundcolor='rgb(230, 230,230)'

# )
# subplot(p1, p2) %>% layout(title = "3D Plot of Figure 1", scene = list(domain=list(x=c(0,0.5),y=c(0,1)),

#                       xaxis=axx, yaxis=axx, zaxis=axx,

#                       aspectmode='cube'),

#          scene2 = list(domain=list(x=c(0.5,1),y=c(0,1)),

#                        xaxis=axx, yaxis=axx, zaxis=axx,

#                        aspectmode='cube')) %>% layout(annotations = list(
#  list(x = 0.2 , y = 0.95, text = "beta1", showarrow = F, xref='paper', yref='paper'),
#   list(x = 0.8 , y = 0.95, text = "beta2", showarrow = F, xref='paper', yref='paper'))
# ) %>%
#                        htmlwidgets::saveWidget("figure1_new.html")

# p2 <- plot_ly(x = x, y = y, z = z2, type = "surface", colors = 'red', showscale = F, showlegend = T, name = 'Mean estimate', scene = 'scene1')
# p3 <- plot_ly(x = x, y = y, z = z3, type = "surface", colors = 'blue', showscale = F, showlegend = T, name = '95% Upper Empirical Band', scene = 'scene1')
# p4 <- plot_ly(x = x, y = y, z = z4, type = "surface", colors = 'blue', showscale = F, showlegend = T, name = '95% Lower Empirical Band', scene = 'scene1')
# p5 <- plot_ly(x = x, y = y, z = z5, type = "surface", colors = 'green', showscale = F, showlegend = T, name = 'Mean of 95% Upper Confidence Band', scene = 'scene1')
# p6 <- plot_ly(x = x, y = y, z = z6, type = "surface", colors = 'green', showscale = F, showlegend = T, name = 'Mean of 95% Lower Confidence Band', scene = 'scene1')



# Wireframe 
df <- data.frame(grid, matrix(true, ncol = 2), 
                 matrix(rowMeans(tvc1), ncol = 2),
                 matrix(rowMeans(tvc1) + 1.96 * rowSds(tvc1), ncol = 2),
                 matrix(rowMeans(tvc1) - 1.96 * rowSds(tvc1), ncol = 2),
                 matrix(rowMeans(tvc1) + 1.96 * rowMeans(sqrt(var_tvc1)), ncol = 2),
                 matrix(rowMeans(tvc1) - 1.96 * rowMeans(sqrt(var_tvc1)), ncol = 2)) %>%
    filter(Var1 + Var2 <= 20)
temp <- c('true', 'est', 'emp upper', 'emp lower', 'ci upper', 'ci lower')
temp2 <- c('beta1', 'beta2')
names(df)[1:2] <- c('x', 'y')
names(df)[3:14] <- expand.grid(temp2, temp) %>% apply(1, function(x)paste(x, collapse = ':'))

df1 <- split(df, df$y)
df2 <- split(df, df$x)

add_mesh <- function(p, legendname, legendgroup, color, columnname, showlegend) {
    # p <- add_trace(p)
    # p <- add_trace(p,
    #     line = list(
    #         color = color, 
    #         width = 2
    #     ), 
    #     x = 0, y = 0, z = 0, 
    #     mode = "lines", 
    #     type = "scatter3d", 
    #     name = legendname, 
    #     legendgroup = legendgroup
    # ) 
    # if (showlegend) {
    #     p <- p %>% layout(showlegend = showlegend)
    # }
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

p1 <- add_mesh(plot_ly(scene = 'scene1'), 'True values', 'group1', 'black', 'beta1:true', TRUE)
p1 <- add_mesh(p1, 'Mean estimate', 'group2', 'red', 'beta1:est', TRUE)
p1 <- add_mesh(p1, '95% Upper Empirical Band', 'group3', 'blue', 'beta1:emp upper', TRUE)
p1 <- add_mesh(p1, 'Mean of 95% Upper Confidence Band', 'group4', 'green', 'beta1:ci upper', TRUE)
p1 <- add_mesh(p1, '95% Lower Empirical Band', 'group5', 'blue', 'beta1:emp lower', TRUE)
p1 <- add_mesh(p1, 'Mean of 95% Lower Confidence Band', 'group6', 'green', 'beta1:ci lower', TRUE)
# p1 <-  p1 %>% layout(scene = list(xaxis = list(range = c(0, 30)), yaxis = list(range = c(0, 30)), zaxis = list(range = c(-1, 1.5))))

p2 <- add_mesh(plot_ly(scene = 'scene2'), 'True values', 'group1', 'black', 'beta2:true', FALSE)
p2 <- add_mesh(p2, 'Mean estimate', 'group2', 'red', 'beta2:est', FALSE)
p2 <- add_mesh(p2, '95% Upper Empirical Band', 'group3', 'blue', 'beta2:emp upper', FALSE)
p2 <- add_mesh(p2, 'Mean of 95% Upper Confidence Band', 'group4', 'green', 'beta2:ci upper', FALSE)
p2 <- add_mesh(p2, '95% Lower Empirical Band', 'group5', 'blue', 'beta2:emp lower', FALSE)
p2 <- add_mesh(p2, 'Mean of 95% Lower Confidence Band', 'group6', 'green', 'beta2:ci lower', FALSE)
# p2 <-  p2 %>% layout(scene = list(xaxis = list(range = c(0, 30)), yaxis = list(range = c(0, 30)), zaxis = list(range = c(-1.5, 2.5))))

subplot(p1, p2) %>% 
layout(title = "3D Plot of Figure 1", 
    scene = list(domain=list(x=c(0,0.5), y=c(0,1)),
        xaxis = list(range = c(0, 20)),
        yaxis = list(range = c(0, 20)),
        zaxis = list(range = c(-1, 1.5)),
        aspectmode='cube'),
    scene2 = list(domain=list(x=c(0.5,1),y=c(0,1)),
        xaxis = list(range = c(0, 20)),
        yaxis = list(range = c(0, 20)),
        zaxis = list(range = c(-1.5, 2.5)),
        aspectmode='cube')) %>% 
layout(annotations = list(
    list(x = 0.2 , y = 0.95, text = "beta1", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.8 , y = 0.95, text = "beta2", showarrow = F, xref='paper', yref='paper'))
) %>%
htmlwidgets::saveWidget("figure1_3d.html")



# add_trace(plot_ly(showlegend = TRUE),
#         line = list(
#             color = 'red', 
#             width = 2
#         ), 
#         x = 0:1, y = 0:1, z = 0:1, 
#         mode = "lines", 
#         type = "scatter3d", 
#         name = 'm', 
#         legendgroup = 'group1',
#         showlegend = TRUE
#     ) %>% layout(showlegend = TRUE)



# p1 <- plot_ly() %>% add_trace(
#     x = 0, y = 0, z = 0, 
#     mode = "lines", 
#     type = "scatter3d", 
#     name = 'True values', 
#     legendgroup = "group1"
# ) %>% layout(showlegend = TRUE)

# #itterate over lines and add them to plot:
# for(i in seq_along(df1)){
#     df_sp <- df1[[i]]
#     p1 <- add_trace(p1, line = list(
#         color = "black", 
#         width = 2
#     ), 
#     mode = "lines", 
#     type = "scatter3d", 
#     x = df_sp$x,
#     y = df_sp$y,
#     z = df_sp[['beta1:true']],
#     showlegend = FALSE,
#     legendgroup = "group1")
# }

# for(i in seq_along(df2)){
#     df_sp <- df2[[i]]
#     p1 <- add_trace(p1, line = list(
#         color = "black", 
#         width = 2
#     ), 
#     mode = "lines", 
#     type = "scatter3d", 
#     x = df_sp$x,
#     y = df_sp$y,
#     z = df_sp[['beta1:true']],
#     showlegend = FALSE,
#     legendgroup = "group1")
# }

# p1 <- add_trace(p1,
#     x = 0, y = 0, z = 0, 
#     mode = "lines", 
#     type = "scatter3d", 
#     name = 'Mean estimate', 
#     legendgroup = "group2"
# ) %>% layout(showlegend = TRUE)

# #itterate over lines and add them to plot:
# for(i in seq_along(df1)){
#     df_sp <- df1[[i]]
#     p1 <- add_trace(p1, line = list(
#         color = "#0066FF", 
#         width = 2
#     ), 
#     mode = "lines", 
#     type = "scatter3d", 
#     x = df_sp$x,
#     y = df_sp$y,
#     z = df_sp[['beta1:est']],
#     showlegend = FALSE,
#     legendgroup = "group2")
# }

# for(i in seq_along(df2)){
#     df_sp <- df2[[i]]
#     p1 <- add_trace(p1, line = list(
#         color = "#0066FF", 
#         width = 2
#     ), 
#     mode = "lines", 
#     type = "scatter3d", 
#     x = df_sp$x,
#     y = df_sp$y,
#     z = df_sp[['beta1:est']],
#     showlegend = FALSE,
#     legendgroup = "group2")
# }
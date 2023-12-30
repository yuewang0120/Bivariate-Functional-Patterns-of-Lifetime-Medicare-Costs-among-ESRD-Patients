suppressMessages({library(ggplot2)
library(dplyr)
library(plotly)})
grid <- expand.grid(seq(0, 3000, 100), seq(0, 3000, 100)) %>% filter(Var1 + Var2 < 3000)
teval <- grid[,1]
seval <- grid[,2]
subset <- (teval + seval) %in% c(800, 1600, 2200)
temp <- readRDS('real_mixed_fit2/all')
coef <- temp$coef
var <- temp$var
png('figure4.png', height=250, width=860)
data.frame(x = teval, coef = c(coef), upper = c(coef + 1.96*sqrt(var)), lower = c(coef - 1.96*sqrt(var)), death = teval + seval) %>%
    filter(subset) %>%
    ggplot(aes(x = x)) + 
    geom_line(aes(y = coef)) + 
    geom_line(aes(y = upper), linetype='dashed', color = 'red') + 
    geom_line(aes(y = lower), linetype='dashed', color = 'red') + 
    geom_line(aes(y = 0), linetype='dotted') + 
    facet_grid(~factor(paste0("'T-S=", death, "'"), levels=paste0("'T-S=", c(800, 1600, 2200), "'")), scales = 'free', space = 'free', labeller = label_parsed) +
    ylab("") +
    xlab("days since entry") +
    theme(text = element_text(size = 20))
dev.off()

temp <- readRDS('real_mixed_fit2/ip')
coef <- temp$coef
var <- temp$var
temp <- readRDS('real_mixed_fit2/op')
coef <- c(coef, temp$coef)
var <- c(var, temp$var)
temp <- readRDS('real_mixed_fit2/sn')
coef <- c(coef, temp$coef)
var <- c(var, temp$var)
temp <- readRDS('real_mixed_fit2/hh')
coef <- c(coef, temp$coef)
var <- c(var, temp$var)
temp <- readRDS('real_mixed_fit2/hs')
coef <- c(coef, temp$coef)
var <- c(var, temp$var)
png('figure5.png', height=1000, width=860)
temp <- c('IP', 'OP', 'SN', 'HH', 'HS')
data.frame(x = rep(teval, 5), y = rep(seval, 5), coef = coef, upper = coef + 1.96*sqrt(var), lower = coef - 1.96*sqrt(var), 
           death = rep(teval + seval, 5), type = rep(factor(temp, levels = temp), each = length(teval))) %>%
    filter((x + y) %in% c(800, 1600, 2200)) %>%
    ggplot(aes(x = x)) + 
    geom_line(aes(y = coef)) + 
    geom_line(aes(y = upper), linetype='dashed', color = 'red') + 
    geom_line(aes(y = lower), linetype='dashed', color = 'red') + 
    geom_line(aes(y = 0), linetype='dotted') + 
    facet_grid(type~factor(paste0("'T-S=", death, "'"), levels=paste0("'T-S=", c(800, 1600, 2200), "'")), scales = 'free', space = 'free_x', labeller = label_parsed) +
    ylab("") +
    xlab("days since entry") +
    theme(text = element_text(size = 20))
dev.off()

## 3d plot
# x <- sort(unique(grid[,1]))
# y <- sort(unique(grid[,2]))
# colid <- match(grid[,1], x)
# rowid <- match(grid[,2], y)
# id <- rowid + (colid - 1) * length(y)
# z1 <- matrix(NA, length(y), length(x))
# z1[id] <- coef
# z2 <- matrix(NA, length(y), length(x))
# z2[id] <- coef + 1.96 * sqrt(var)
# z3 <- matrix(NA, length(y), length(x))
# z3[id] <- coef - 1.96 * sqrt(var)

# plot_ly(x = x, y = y, showscale = F, showlegend = T) %>% 
# add_surface(z = z1, name = 'Estimate', colorscale = list(c(0, 1), c("black", "black")), opacity = 0.5) %>%
# add_surface(z = z2, name = '95% Upper Confidence Band', colorscale = list(c(0, 1), c("red", "red")), opacity = 0.5) %>%
# add_surface(z = z3, name = '95% Lower Confidence Band', colorscale = list(c(0, 1), c("red", "red")), opacity = 0.5) %>%
# layout(title = "3D Plot of Figure 4") %>%
# htmlwidgets::saveWidget("figure4_3d.html")

# temp <- readRDS('real_mixed_fit2/ip')
# coef <- temp$coef
# var <- temp$var
# z1 <- matrix(NA, length(y), length(x))
# z1[id] <- coef
# z2 <- matrix(NA, length(y), length(x))
# z2[id] <- coef + 1.96 * sqrt(var)
# z3 <- matrix(NA, length(y), length(x))
# z3[id] <- coef - 1.96 * sqrt(var)
# temp <- readRDS('real_mixed_fit2/op')
# coef <- temp$coef
# var <- temp$var
# z4 <- matrix(NA, length(y), length(x))
# z4[id] <- coef
# z5 <- matrix(NA, length(y), length(x))
# z5[id] <- coef + 1.96 * sqrt(var)
# z6 <- matrix(NA, length(y), length(x))
# z6[id] <- coef - 1.96 * sqrt(var)
# temp <- readRDS('real_mixed_fit2/sn')
# coef <- temp$coef
# var <- temp$var
# z7 <- matrix(NA, length(y), length(x))
# z7[id] <- coef
# z8 <- matrix(NA, length(y), length(x))
# z8[id] <- coef + 1.96 * sqrt(var)
# z9 <- matrix(NA, length(y), length(x))
# z9[id] <- coef - 1.96 * sqrt(var)
# temp <- readRDS('real_mixed_fit2/hh')
# coef <- temp$coef
# var <- temp$var
# z10 <- matrix(NA, length(y), length(x))
# z10[id] <- coef
# z11 <- matrix(NA, length(y), length(x))
# z11[id] <- coef + 1.96 * sqrt(var)
# z12 <- matrix(NA, length(y), length(x))
# z12[id] <- coef - 1.96 * sqrt(var)
# temp <- readRDS('real_mixed_fit2/hs')
# coef <- temp$coef
# var <- temp$var
# z13 <- matrix(NA, length(y), length(x))
# z13[id] <- coef
# z14 <- matrix(NA, length(y), length(x))
# z14[id] <- coef + 1.96 * sqrt(var)
# z15 <- matrix(NA, length(y), length(x))
# z15[id] <- coef - 1.96 * sqrt(var)

# p1 <- plot_ly(x = x, y = y, showscale = F, showlegend = T, scene = 'scene1') %>% 
# add_surface(z = z1, colorscale = list(c(0, 1), c("black", "black")), name = 'IP: Estimate', opacity = 0.5) %>%
# add_surface(z = z2, colorscale = list(c(0, 1), c("red", "red")), name = 'IP: 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z3, colorscale = list(c(0, 1), c("red", "red")), name = 'IP: 95% Lower Confidence Band', opacity = 0.5)
# p2 <- plot_ly(x = x, y = y, showscale = F, showlegend = T, scene = 'scene2') %>% 
# add_surface(z = z4, colorscale = list(c(0, 1), c("black", "black")), name = 'OP: Estimate', opacity = 0.5) %>%
# add_surface(z = z5, colorscale = list(c(0, 1), c("red", "red")), name = 'OP: 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z6, colorscale = list(c(0, 1), c("red", "red")), name = 'OP: 95% Lower Confidence Band', opacity = 0.5)
# p3 <- plot_ly(x = x, y = y, showscale = F, showlegend = T, scene = 'scene3') %>% 
# add_surface(z = z7, colorscale = list(c(0, 1), c("black", "black")), name = 'SN: Estimate', opacity = 0.5) %>%
# add_surface(z = z8, colorscale = list(c(0, 1), c("red", "red")), name = 'SN: 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z9, colorscale = list(c(0, 1), c("red", "red")), name = 'SN: 95% Lower Confidence Band', opacity = 0.5)
# p4 <- plot_ly(x = x, y = y, showscale = F, showlegend = T, scene = 'scene4') %>% 
# add_surface(z = z10, colorscale = list(c(0, 1), c("black", "black")), name = 'HH: Estimate', opacity = 0.5) %>%
# add_surface(z = z11, colorscale = list(c(0, 1), c("red", "red")), name = 'HH: 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z12, colorscale = list(c(0, 1), c("red", "red")), name = 'HH: 95% Lower Confidence Band', opacity = 0.5)
# p5 <- plot_ly(x = x, y = y, showscale = F, showlegend = T, scene = 'scene5') %>% 
# add_surface(z = z13, colorscale = list(c(0, 1), c("black", "black")), name = 'HS: Estimate', opacity = 0.5) %>%
# add_surface(z = z14, colorscale = list(c(0, 1), c("red", "red")), name = 'HS: 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z15, colorscale = list(c(0, 1), c("red", "red")), name = 'HS: 95% Lower Confidence Band', opacity = 0.5)


# axx <- list(
#   gridcolor='rgb(255, 255, 255)',

#   zerolinecolor='rgb(255, 255, 255)',

#   showbackground=TRUE,

#   backgroundcolor='rgb(230, 230,230)'

# )
# subplot(p1, p2, p3, p4, p5) %>% layout(title = "3D Plot of Figure 5", scene = list(domain=list(x=c(0,0.33),y=c(0.5,1)),

#                       xaxis=axx, yaxis=axx, zaxis=axx,

#                       aspectmode='cube'),

#          scene2 = list(domain=list(x=c(0.33,0.66),y=c(0.5,1)),

#                        xaxis=axx, yaxis=axx, zaxis=axx,

#                        aspectmode='cube'),
#         scene3 = list(domain=list(x=c(0.66,1),y=c(0.5,1)),

#                        xaxis=axx, yaxis=axx, zaxis=axx,

#                        aspectmode='cube'),
#         scene4 = list(domain=list(x=c(0,0.33),y=c(0,0.5)),

#                        xaxis=axx, yaxis=axx, zaxis=axx,

#                        aspectmode='cube'),
#         scene5 = list(domain=list(x=c(0.33,0.66),y=c(0,0.5)),

#                        xaxis=axx, yaxis=axx, zaxis=axx,

#                        aspectmode='cube')) %>% layout(annotations = list(
#  list(x = 0.166 , y = 0.975, text = "IP", showarrow = F, xref='paper', yref='paper'),
#  list(x = 0.5, y = 0.975, text = "OP", showarrow = F, xref='paper', yref='paper'),
#  list(x = 0.833 , y = 0.975, text = "SN", showarrow = F, xref='paper', yref='paper'),
#  list(x = 0.166 , y = 0.475, text = "HH", showarrow = F, xref='paper', yref='paper'),
#   list(x = 0.5 , y = 0.475, text = "HS", showarrow = F, xref='paper', yref='paper'))
# ) %>%
#                        htmlwidgets::saveWidget("figure5_3d.html")






# quantiles of T - S
# load('../../cohort3_10p.RData')
# data %>% group_by(USRDS_ID) %>% summarize(T = first(as.numeric(DIED - CAN_FIRST_LISTING_DT))) %>% with(quantile(T, 0.85))


# Wireframe
temp <- readRDS('real_mixed_fit2/all')
coef <- temp$coef
var <- temp$var

df <- data.frame(grid, coef, coef + 1.96 * sqrt(var), coef - 1.96 * sqrt(var))
names(df) <- c('x', 'y', 'est', 'ci upper', 'ci lower')

df1 <- split(df, df$y)
df2 <- split(df, df$x)
# for (i in 1:length(df1)) {
#     if (any(diff(df1[[i]][['x']])!=100)) stop()
# }
# for (i in 1:length(df2)) {
#     if (any(diff(df2[[i]][['y']])!=100)) stop()
# }
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
p1 <- add_mesh(plot_ly(), 'Estimate', 'group1', 'black', 'est', TRUE)
p1 <- add_mesh(p1, '95% Upper Confidence Band', 'group2', 'red', 'ci upper', TRUE)
p1 <- add_mesh(p1, '95% Lower Confidence Band', 'group3', 'red', 'ci lower', TRUE)
p1 %>% 
layout(title = "3D Plot of Figure 4", 
    scene = list(domain=list(x=c(0,1), y=c(0,1)),
        xaxis = list(range = c(0, 3000)),
        yaxis = list(range = c(0, 3000)),
        zaxis = list(range = c(-1, 3.5)),
        aspectmode='cube')) %>% 
layout(annotations = list(
    list(x = 0.5 , y = 0.95, text = "gamma", showarrow = F, xref='paper', yref='paper'))
) %>%
htmlwidgets::saveWidget("figure4_3d.html") %>% suppressWarnings()


df <- data.frame(grid)
names(df) <- c('x', 'y')
temp <- readRDS('real_mixed_fit2/ip')
coef <- temp$coef
var <- temp$var
df[['ip:est']] <- coef
df[['ip:ci upper']] <- coef + 1.96 * sqrt(var)
df[['ip:ci lower']] <- coef + 1.96 * sqrt(var)
temp <- readRDS('real_mixed_fit2/op')
coef <- temp$coef
var <- temp$var
df[['op:est']] <- coef
df[['op:ci upper']] <- coef + 1.96 * sqrt(var)
df[['op:ci lower']] <- coef + 1.96 * sqrt(var)
temp <- readRDS('real_mixed_fit2/sn')
coef <- temp$coef
var <- temp$var
df[['sn:est']] <- coef
df[['sn:ci upper']] <- coef + 1.96 * sqrt(var)
df[['sn:ci lower']] <- coef + 1.96 * sqrt(var)
temp <- readRDS('real_mixed_fit2/hh')
coef <- temp$coef
var <- temp$var
df[['hh:est']] <- coef
df[['hh:ci upper']] <- coef + 1.96 * sqrt(var)
df[['hh:ci lower']] <- coef + 1.96 * sqrt(var)
temp <- readRDS('real_mixed_fit2/hs')
coef <- temp$coef
var <- temp$var
df[['hs:est']] <- coef
df[['hs:ci upper']] <- coef + 1.96 * sqrt(var)
df[['hs:ci lower']] <- coef + 1.96 * sqrt(var)
df1 <- split(df, df$y)
df2 <- split(df, df$x)


p1 <- add_mesh(plot_ly(scene = 'scene1'), 'Estimate', 'group1', 'black', 'ip:est', TRUE)
p1 <- add_mesh(p1, '95% Upper Confidence Band', 'group2', 'red', 'ip:ci upper', TRUE)
p1 <- add_mesh(p1, '95% Lower Confidence Band', 'group3', 'red', 'ip:ci lower', TRUE)

p2 <- add_mesh(plot_ly(scene = 'scene2'), 'Estimate', 'group1', 'black', 'op:est', FALSE)
p2 <- add_mesh(p2, '95% Upper Confidence Band', 'group2', 'red', 'op:ci upper', FALSE)
p2 <- add_mesh(p2, '95% Lower Confidence Band', 'group3', 'red', 'op:ci lower', FALSE)

p3 <- add_mesh(plot_ly(scene = 'scene3'), 'Estimate', 'group1', 'black', 'sn:est', FALSE)
p3 <- add_mesh(p3, '95% Upper Confidence Band', 'group2', 'red', 'sn:ci upper', FALSE)
p3 <- add_mesh(p3, '95% Lower Confidence Band', 'group3', 'red', 'sn:ci lower', FALSE)

p4 <- add_mesh(plot_ly(scene = 'scene4'), 'Estimate', 'group1', 'black', 'hh:est', FALSE)
p4 <- add_mesh(p4, '95% Upper Confidence Band', 'group2', 'red', 'hh:ci upper', FALSE)
p4 <- add_mesh(p4, '95% Lower Confidence Band', 'group3', 'red', 'hh:ci lower', FALSE)

p5 <- add_mesh(plot_ly(scene = 'scene5'), 'Estimate', 'group1', 'black', 'hs:est', FALSE)
p5 <- add_mesh(p5, '95% Upper Confidence Band', 'group2', 'red', 'hs:ci upper', FALSE)
p5 <- add_mesh(p5, '95% Lower Confidence Band', 'group3', 'red', 'hs:ci lower', FALSE)

subplot(p1, p2, p3, p4, p5) %>% 
layout(title = "3D Plot of Figure 5", 
    scene = list(domain=list(x=c(0, 0.33), y=c(0.5, 1)),
        xaxis = list(range = c(0, 3000)),
        yaxis = list(range = c(0, 3000)),
        zaxis = list(range = c(-0.5, 4.5)),
        aspectmode='cube'),
    scene2 = list(domain=list(x=c(0.33, 0.66), y=c(0.5, 1)),
        xaxis = list(range = c(0, 3000)),
        yaxis = list(range = c(0, 3000)),
        zaxis = list(range = c(-1, 0)),
        aspectmode='cube'),
    scene3 = list(domain=list(x=c(0.66, 1), y=c(0.5,1)),
        xaxis = list(range = c(0, 3000)),
        yaxis = list(range = c(0, 3000)),
        zaxis = list(range = c(-0.5, 0.5)),
        aspectmode='cube'),
    scene4 = list(domain=list(x=c(0, 0.33), y=c(0, 0.5)),
        xaxis = list(range = c(0, 3000)),
        yaxis = list(range = c(0, 3000)),
        zaxis = list(range = c(-0.5, 0.5)),
        aspectmode='cube'),
    scene5 = list(domain=list(x=c(0.33, 0.66), y=c(0, 0.5)),
        xaxis = list(range = c(0, 3000)),
        yaxis = list(range = c(0, 3000)),
        zaxis = list(range = c(-0.5, 1)),
        aspectmode='cube')) %>% 
layout(annotations = list(
    list(x = 0.166 , y = 0.975, text = "IP", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.5, y = 0.975, text = "OP", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.833 , y = 0.975, text = "SN", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.166 , y = 0.475, text = "HH", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.5 , y = 0.475, text = "HS", showarrow = F, xref='paper', yref='paper'))
) %>%
htmlwidgets::saveWidget("figure5_3d.html") %>% suppressWarnings()


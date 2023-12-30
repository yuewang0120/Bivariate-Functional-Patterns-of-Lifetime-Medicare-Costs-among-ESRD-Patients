suppressMessages({library(dplyr)
library(plotly)
library(ggplot2)})
load('real_semi_fit')
print('Table 3')
temp <- cbind(fc2, fc2 - 1.96 * sqrt(diag(var_fc2)), fc2 + 1.96 * sqrt(diag(var_fc2)), 2*pnorm(-abs(fc2), sd = sqrt(diag(var_fc2)))) %>% signif(4) %>% '*'(1)
colnames(temp) <- c('Estimate', 'Lower CI', 'Upper CI', 'P-value')
rownames(temp) <- c('Waitlisted', 'Time X Waitlisted', 'Race: black', 'Race: other', 'Sex: female', 'Age', 'Hypertension', 'Other comorbidities', '25 <= BMI < 30', 'BMI >= 30')
print(temp)



png('figure3.png', height=500, width=860)
grid <- expand.grid(seq(0, 3000, 50), seq(0, 3000, 50)) %>% filter(Var1 + Var2 <= 3000)
teval <- grid$Var1
seval <- grid$Var2
data.frame(x = rep(teval, 2), y = rep(seval, 2), est = c(tvc1), 
           upper = c(tvc1 + 1.96*sqrt(var_tvc1[, c(1,3)])),
           lower = c(tvc1 - 1.96*sqrt(var_tvc1[, c(1,3)])),
           death = rep(teval + seval, 2), beta = rep(paste0('beta[', 1:2, ']'), each = length(teval))) %>%
    filter((x + y) %in% c(500, 1250, 2000)) %>%
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

## 3d plot
# grid <- expand.grid(seq(0, 3000, 50), seq(0, 3000, 50)) %>% filter(Var1 + Var2 < 3000)
# x <- sort(unique(grid[,1]))
# y <- sort(unique(grid[,2]))
# colid <- match(grid[,1], x)
# rowid <- match(grid[,2], y)
# id <- rowid + (colid - 1) * length(y)
# z1 <- matrix(NA, length(y), length(x))
# z1[id] <- tvc1[,1]
# z2 <- matrix(NA, length(y), length(x))
# z2[id] <- tvc1[,1] + 1.96 * sqrt(var_tvc1[,1])
# z3 <- matrix(NA, length(y), length(x))
# z3[id] <- tvc1[,1] - 1.96 * sqrt(var_tvc1[,1])
# z4 <- matrix(NA, length(y), length(x))
# z4[id] <- tvc1[,2]
# z5 <- matrix(NA, length(y), length(x))
# z5[id] <- tvc1[,2] + 1.96 * sqrt(var_tvc1[,3])
# z6 <- matrix(NA, length(y), length(x))
# z6[id] <- tvc1[,2] - 1.96 * sqrt(var_tvc1[,3])

# p1 <- plot_ly(x = x, y = y, showscale = F, showlegend = T, scene = 'scene1') %>% 
# # add_surface(z = z1, name = 'True values', colorscale = 'RdBu')
# add_surface(z = z1, colorscale = list(c(0, 1), c("black", "black")), name = 'beta1: Estimate', opacity = 0.5) %>%
# add_surface(z = z2, colorscale = list(c(0, 1), c("red", "red")), name = 'beta1: 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z3, colorscale = list(c(0, 1), c("red", "red")), name = 'beta1: 95% Lower Confidence Band', opacity = 0.5)
# p2 <- plot_ly(x = x, y = y, showscale = F, showlegend = T, scene = 'scene2') %>% 
# # add_surface(z = z4, name = 'True values', colorscale = 'RdBu')
# add_surface(z = z4, colorscale = list(c(0, 1), c("black", "black")), name = 'beta2: Estimate', opacity = 0.5) %>%
# add_surface(z = z5, colorscale = list(c(0, 1), c("red", "red")), name = 'beta2: 95% Upper Confidence Band', opacity = 0.5) %>%
# add_surface(z = z6, colorscale = list(c(0, 1), c("red", "red")), name = 'beta2: 95% Lower Confidence Band', opacity = 0.5)
# axx <- list(

#   gridcolor='rgb(255, 255, 255)',

#   zerolinecolor='rgb(255, 255, 255)',

#   showbackground=TRUE,

#   backgroundcolor='rgb(230, 230,230)'

# )
# subplot(p1, p2) %>% layout(title = "3D Plot of Figure 3", scene = list(domain=list(x=c(0,0.5),y=c(0,1)),

#                       xaxis=axx, yaxis=axx, zaxis=axx,

#                       aspectmode='cube'),

#          scene2 = list(domain=list(x=c(0.5,1),y=c(0,1)),

#                        xaxis=axx, yaxis=axx, zaxis=axx,

#                        aspectmode='cube')) %>% layout(annotations = list(
#  list(x = 0.2 , y = 0.95, text = "beta1", showarrow = F, xref='paper', yref='paper'),
#   list(x = 0.8 , y = 0.95, text = "beta2", showarrow = F, xref='paper', yref='paper'))
# ) %>%
#                        htmlwidgets::saveWidget("figure3_3d.html")




# comparison test table
# load('real_semi_fit_for_test')
print('Table 4')
temp <- matrix(NA, 3, 4)
t1 <- 50
s1 <- 450
t2 <- 250
s2 <- 250
id1 <- with(grid, Var1 == t1 & Var2 == s1)
id2 <- with(grid, Var1 == t2 & Var2 == s2)
temp[1,] <- c(tvc1[id2,2] - tvc1[id1,2] + sqrt(var_tvc1[id1,3] + var_tvc1[id2,3]) * 1.96 * c(0,-1,1),
pnorm((tvc1[id2,2] - tvc1[id1,2]) / sqrt(var_tvc1[id1,3] + var_tvc1[id2,3]), lower.tail = F))
t1 <- 50
s1 <- 1200
t2 <- 250
s2 <- 1000
id1 <- with(grid, Var1 == t1 & Var2 == s1)
id2 <- with(grid, Var1 == t2 & Var2 == s2)
temp[2,] <- c(tvc1[id2,2] - tvc1[id1,2] + sqrt(var_tvc1[id1,3] + var_tvc1[id2,3]) * 1.96 * c(0,-1,1),
pnorm((tvc1[id2,2] - tvc1[id1,2]) / sqrt(var_tvc1[id1,3] + var_tvc1[id2,3]), lower.tail = F))
t1 <- 50
s1 <- 1950
t2 <- 250
s2 <- 1750
id1 <- with(grid, Var1 == t1 & Var2 == s1)
id2 <- with(grid, Var1 == t2 & Var2 == s2)
temp[3,] <- c(tvc1[id2,2] - tvc1[id1,2] + sqrt(var_tvc1[id1,3] + var_tvc1[id2,3]) * 1.96 * c(0,-1,1),
pnorm((tvc1[id2,2] - tvc1[id1,2]) / sqrt(var_tvc1[id1,3] + var_tvc1[id2,3]), lower.tail = F))
colnames(temp) <- c('Estimated difference', 'Lower CI', 'Upper CI', 'P-value')
rownames(temp) <- paste0('T=', c(500, 1250, 2000))
print(temp)


# find the quantiles of T
# load('../../cohort2_10p.RData')
# data2 <- data
# load('../../cohort1_10p.RData')
# data <- data %>% bind_rows(data2)
# data %>% group_by(USRDS_ID) %>% summarize(T = first(as.numeric(DIED - FIRST_SE))) %>% with(quantile(T, 0.89))



# Wireframe
df <- data.frame(grid, tvc1, 
                tvc1 + 1.96 * sqrt(var_tvc1[,c(1,3)]),
                tvc1 - 1.96 * sqrt(var_tvc1[,c(1,3)])) %>% filter(Var1 + Var2 <= 2600)
names(df) <- c('x', 'y', 'beta1:est', 'beta2:est', 
    'beta1:ci upper', 'beta2:ci upper', 'beta1:ci lower', 'beta2:ci lower')

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
p1 <- add_mesh(plot_ly(scene = 'scene1'), 'Estimate', 'group1', 'black', 'beta1:est', TRUE)
p1 <- add_mesh(p1, '95% Upper Confidence Band', 'group2', 'red', 'beta1:ci upper', TRUE)
p1 <- add_mesh(p1, '95% Lower Confidence Band', 'group3', 'red', 'beta1:ci lower', TRUE)

p2 <- add_mesh(plot_ly(scene = 'scene2'), 'Estimate', 'group1', 'black', 'beta2:est', FALSE)
p2 <- add_mesh(p2, '95% Upper Confidence Band', 'group2', 'red', 'beta2:ci upper', FALSE)
p2 <- add_mesh(p2, '95% Lower Confidence Band', 'group3', 'red', 'beta2:ci lower', FALSE)


subplot(p1, p2) %>% 
layout(title = "3D Plot of Figure 3", 
    scene = list(domain=list(x=c(0,0.5), y=c(0,1)),
        xaxis = list(range = c(0, 2600)),
        yaxis = list(range = c(0, 2600)),
        zaxis = list(range = c(0, 2)),
        aspectmode='cube'),
    scene2 = list(domain=list(x=c(0.5,1),y=c(0,1)),
        xaxis = list(range = c(0, 2600)),
        yaxis = list(range = c(0, 2600)),
        zaxis = list(range = c(-1.5, 1.5)),
        aspectmode='cube')) %>% 
layout(annotations = list(
    list(x = 0.2 , y = 0.95, text = "beta1", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.8 , y = 0.95, text = "beta2", showarrow = F, xref='paper', yref='paper'))
) %>%
htmlwidgets::saveWidget("figure3_3d.html") %>% suppressWarnings()

print('Note these tables are obtained from the pseudo dataset and are not exactly the same as the tables in the paper.')

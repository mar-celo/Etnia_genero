library(crosstalk)
library(ggplot2)
library(gapminder)
library(plotly)

sd <- highlight_key(gapminder)

g <- ggplot(sd, aes(year, lifeExp, color = country, group = country)) +
  geom_col()

bscols(widths = c(12, 12),
       filter_select("country", "Country:", sd, ~ country),
       ggplotly(g)
)


library(dplyr)
iris%>%
  group_by(Species) %>%
  group_map(~ plot_ly(data=., x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = "scatter", mode="markers"), keep=TRUE) %>%
  subplot(nrows = 1, shareX = TRUE, shareY=TRUE)

library(plotly)

y <- c('The course was effectively<br>organized',
       'The course developed my<br>abilities and skills for<br>the subject',
       'The course developed my<br>ability to think critically about<br>the subject',
       'I would recommend this<br>course to a friend')
x1 <- c(21, 24, 27, 29)
x2 <-c(30, 31, 26, 24)
x3 <- c(21, 19, 23, 15)
x4 <- c(16, 15, 11, 18)
x5 <- c(12, 11, 13, 14)

data <- data.frame(y, x1, x2, x3, x4, x5)

top_labels <- c('Strongly<br>agree', 'Agree', 'Neutral', 'Disagree', 'Strongly<br>disagree')

fig <- plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
               marker = list(color = 'rgba(38, 24, 74, 0.8)',
                             line = list(color = 'rgb(248, 248, 249)', width = 1))) 
fig <- fig %>% add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) 
fig <- fig %>% add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) 
fig <- fig %>% add_trace(x = ~x4, marker = list(color = 'rgba(164, 163, 204, 0.85)')) 
fig <- fig %>% add_trace(x = ~x5, marker = list(color = 'rgba(190, 192, 213, 1)')) 
fig <- fig %>% layout(xaxis = list(title = "",
                                   showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = FALSE,
                                   domain = c(0.15, 1)),
                      yaxis = list(title = "",
                                   showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = FALSE),
                      barmode = 'stack',
                      paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
                      margin = list(l = 120, r = 10, t = 140, b = 80),
                      showlegend = FALSE) 
# labeling the y-axis
fig <- fig %>% add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                               xanchor = 'right',
                               text = y,
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(67, 67, 67)'),
                               showarrow = FALSE, align = 'right') 
# labeling the percentages of each bar (x_axis)
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 / 2, y = y,
                               text = paste(data[,"x1"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 + x2 / 2, y = y,
                               text = paste(data[,"x2"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 + x2 + x3 / 2, y = y,
                               text = paste(data[,"x3"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 + x2 + x3 + x4 / 2, y = y,
                               text = paste(data[,"x4"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 + x2 + x3 + x4 + x5 / 2, y = y,
                               text = paste(data[,"x5"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
# labeling the first Likert scale (on the top)
fig <- fig %>% add_annotations(xref = 'x', yref = 'paper',
                               x = c(21 / 2, 21 + 30 / 2, 21 + 30 + 21 / 2, 21 + 30 + 21 + 16 / 2,
                                     21 + 30 + 21 + 16 + 12 / 2),
                               y = 1.15,
                               text = top_labels,
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(67, 67, 67)'),
                               showarrow = FALSE)

fig



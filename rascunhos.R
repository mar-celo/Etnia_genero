# Rascunho ----------------------------------------------------------------


tab2 <- tabela |>
  tidyr::pivot_wider(names_from =Sexo,
                     values_from = total) |> 
  mutate(Total = Fem + Mas) |> 
  filter(
    `Orgão Vinculado (Cargos e Funçõe` == "Advocacia-Geral Da Uniao"
  ) 

tab3 <- tabela |> 
  filter(
    `Orgão Vinculado (Cargos e Funçõe` == "Advocacia-Geral Da Uniao",
    
  ) 


ggplot(tab3) +
  aes(x = `Orgão Vinculado (Cargos e Funçõe`, y = total, fill = `Nome Cor Origem Etnica`) +
  geom_col() +
  scale_fill_manual(values = c(AMARELA = "#F8766D", BRANCA = "#93AA00", `NAO INFORMADO` = "#00C19F",
                               PARDA = "#619CFF", PRETA = "#FF61C3")) +
  labs(x = "x", y = "y", title = "Titulo", subtitle = "Subtitle",
       caption = "caption", fill = "Origem Étnica") +
  theme_minimal() +
  facet_wrap(vars(Sexo)) 
#coord_flip() +

tab3 |> group_by(`Nome Cor Origem Etnica`) |> 
  do(p=plot_ly(., x = ~`Orgão Vinculado (Cargos e Funçõe`, 
               y = ~total, 
               color = ~Sexo, type = "bar")) |> 
  subplot(nrows = 3, shareX = TRUE, shareY = TRUE)


l <- list(
  font = list(
    family = "sans-serif",
    size = 10,
    color = "green"),
  bgcolor = "#E2E2E2",
  bordercolor = "white",
  x = 0, y = 1,
  orientation = "h",
  borderwidth = 3,
  title=list(text='<b> Cargos e Funções </b>'))

library("RColorBrewer")

plot_ly(tab3, x = ~`Orgão Vinculado (Cargos e Funçõe`, 
        y = ~total, 
        color = ~`Nome Cor Origem Etnica`, 
        colors = brewer.pal(n = 8, name = "PuBu")
)  |> 
  add_bars(  ) |> 
  layout(barmode = "group",
         title = "A Fruity Bar Plot",
         legend = l)

library(plotly)
iris |> 
  group_by(Species)  |> 
  do(p=plot_ly(., x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = "scatter")) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)


c("#FFF7FB", "#ECE7F2", "#D0D1E6", "#A6BDDB", "#74A9CF", "#3690C0", "#0570B0", "#034E7B")


# Grafico de linhas -------------------------------------------------------

library(tidyquant)
library(plotly)
tickers = c("GOOG", "AAPL", "AMZN",  "NFLX", "MSFT")
for (i in tickers){
  getSymbols(i,
             from = "2018-01-01",
             to = "2019-12-31")}

x <- list(
  title = "date"
)
y <- list(
  title = "value"
)

stock <- data.frame(GOOG$GOOG.Adjusted,
                    AAPL$AAPL.Adjusted,
                    AMZN$AMZN.Adjusted,
                    NFLX$NFLX.Adjusted,
                    MSFT$MSFT.Adjusted)
stock$GOOG.Adjusted <- stock$GOOG.Adjusted/stock$GOOG.Adjusted[1]
stock$AAPL.Adjusted <- stock$AAPL.Adjusted/stock$AAPL.Adjusted[1]
stock$AMZN.Adjusted <- stock$AMZN.Adjusted/stock$AMZN.Adjusted[1]
stock$NFLX.Adjusted <- stock$NFLX.Adjusted/stock$NFLX.Adjusted[1]
stock$MSFT.Adjusted <- stock$MSFT.Adjusted/stock$MSFT.Adjusted[1]
stock <- data.frame(stock,rownames(stock))
colnames(stock) <- append(tickers,'Dates')

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE
)

fig1 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~GOOG, name = 'GOOG')%>%
  layout(legend=list(title=list(text='company')), xaxis = ax, yaxis = list(range = c(0.5,2), title = 'value'))


fig2 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~AAPL, name = 'AAPL')%>%
  layout(legend=list(title=list(text='company')), xaxis = ax, yaxis = list(range = c(0.5,2),title = '', showticklabels = FALSE))


fig3 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~AMZN, name = 'AMZN')%>%
  layout(legend=list(title=list(text='company')), xaxis = ax, yaxis = list(range = c(0.5,2), title = 'value'))


# fig4 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
#   add_trace(x = ~Dates, y = ~FB, name = 'FB')%>%
#   layout(legend=list(title=list(text='company')), xaxis = ax, yaxis = list(range = c(0.5,2),title = '', showticklabels = FALSE))


fig5 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~NFLX, name = 'NFLX')%>%
  layout(legend=list(title=list(text='company')), xaxis = list(title = 'Date'), yaxis = list(range = c(0.5,2), title = 'value'))


fig6 <- plot_ly(stock, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Dates, y = ~MSFT, name = 'MSFT')%>%
  layout( legend=list(title=list(text='company')), yaxis = list(range = c(0.5,2) ,showticklabels = FALSE, title =''),  xaxis = list(title = 'Date'))


fig <- subplot(fig1, fig2, fig3,  fig5, fig6,
               nrows = 3, titleY = TRUE, titleX = TRUE) %>% layout(
                 xaxis = list(zerolinecolor = '#ffff',
                              zerolinewidth = 2,
                              gridcolor = 'ffff'),
                 yaxis = list(zerolinecolor = '#ffff',
                              zerolinewidth = 2,
                              gridcolor = 'ffff'),
                 plot_bgcolor='#e5ecf6')
annotations = list(
  list(
    x = 0.225,
    y = 1.0,
    font = list(size = 10),
    text = "company=GOOG",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.775,
    y = 1,
    font = list(size = 10),
    text = "company=AAPL",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.225,
    y = 0.64,
    font = list(size = 10),
    text = "company=AMZN",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  # list(
  #   x = 0.775,
  #   y = 0.64,
  #   font = list(size = 10),
  #   text = "company=FB",
  #   xref = "paper",
  #   yref = "paper",
  #   xanchor = "center",
  #   yanchor = "bottom",
  #   showarrow = FALSE
  # ),
  list(
    x = 0.225,
    y = 0.315,
    font = list(size = 10),
    text = "company=NFLX",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.775,
    y = 0.315,
    font = list(size = 10),
    text = "company=MSFT",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
)

fig <- fig %>%layout(annotations = annotations, width = 900)
options(warn = -1)
fig


# Pie Plotly --------------------------------------------------------------


library(plotly)
library(dplyr)

fig <- plot_ly()
fig <- fig %>% add_pie(data = count(diamonds, cut), labels = ~cut, values = ~n,
                       name = "Cut", domain = list(x = c(0, 0.4), y = c(0.4, 1)))
fig <- fig %>% add_pie(data = count(diamonds, color), labels = ~color, values = ~n,
                       name = "Color", domain = list(x = c(0.6, 1), y = c(0.4, 1)))
fig <- fig %>% add_pie(data = count(diamonds, clarity), labels = ~clarity, values = ~n,
                       name = "Clarity", domain = list(x = c(0.25, 0.75), y = c(0, 0.6)))
fig <- fig %>% layout(title = "Pie Charts with Subplots", showlegend = F,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig


USPersonalExpenditure <- data.frame("Categorie" = rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[, c('Categorie', 'X1960')]

df <-  tabela |> group_by(Etnia) |> 
  dplyr::summarise(
  total = sum(Total, na.rm = TRUE)
) |> ungroup()


fig <- plot_ly()
fig <- fig %>% add_pie(data = df, labels = ~Etnia, values = ~total,
                       name = "Feminino", 
                       domain = list(x = c(0, 0.4), y = c(0.4, 1)))

fig <- fig %>% add_pie(data = df, labels = ~Etnia, values = ~total,
          name = "Masculino", domain = list(x = c(0.6, 1), y = c(0.4, 1)))
fig <- fig %>% add_pie(data = df, labels = ~Etnia, values = ~total,
          name = "Total", domain = list(x = c(0.25, 0.75), y = c(0, 0.6)))
fig <- fig %>% layout(title = "Pie Charts with Subplots", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

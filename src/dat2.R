library(dplyr)
library(readxl)
library(readr)

#Filtro base Funções.qvd mes de março

funcoes_mai_23 <- readr::read_delim("src/Funções_0523.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
                             

# Etnia = Negras(Preta +Pardo) e demais

funcoes_mai_23 <- funcoes_mai_23 |> mutate(
  Etnia = case_when( 
    `Cor Origem Etnica` %in% c(4, 6)  ~ "Negras",
    .default = "Demais Raça/Cor"
  )
) |> janitor::clean_names()

# DE_PARA_FUNCOES <- read_excel("src/DE-PARA_FUNCOES.xlsx")|> janitor::clean_names()
# 
# funcoes_mar_23 <- funcoes_mar_23 |> left_join(DE_PARA_FUNCOES, by = c("Nível Função2" = "funcao")) 

# tratamento da base

tabela <- funcoes_mai_23 |> group_by(
  natureza_juridica,
  orgao_vinculado_cargos_e_funcoes,
  orgao_superior_cargos_e_funcoes,
  etnia ,sexo, decreto_nivel) |> 
  dplyr::summarise(
    total = sum(quantidade_de_vinculos_cargos_e_funcoes)
  ) |> ungroup() |> 
  rename(
    `Órgão Superior` = orgao_superior_cargos_e_funcoes,
    `Órgão` = orgao_vinculado_cargos_e_funcoes,
    `Cargo-Função` = decreto_nivel
  ) |> 
  tidyr::pivot_wider(names_from =sexo,
                     values_from = total) |> 
  rowwise()  |> 
  mutate(
    Total = sum(c_across(Fem:Mas), na.rm = TRUE
    )
  ) |> 
  filter(`Cargo-Função` %in% c("Nível 1 a 12", "Nível 13 a 17"))



# Percentual das funcoes por etnia

tabela <- tabela |>
  # filter(Orgão == "Advocacia-Geral Da Uniao",
  #        `Cargo-Função` == "FCPE & FEX"
  #        ) |> 
  group_by(Orgão,  `Cargo-Função`) |> 
  mutate(
    `% Cargo-Função/Etnia` = scales::percent(Total/sum(Total, na.rm = TRUE))
  ) 





# base para gerar mapas
#DADOS_LAT_LONG_ESTADOS <- read_excel("src/DADOS_LAT_LONG_ESTADOS.xlsx")

#Tab <- tabela |> left_join(DADOS_LAT_LONG_ESTADOS, 
#                           join_by(`UF da Organização` == UF) ) 

# Salvar base tratada
saveRDS(tabela, "data/Tab.rds")


# Dados Gerais ------------------------------------------------------------

Etnia_funcao <- read_excel("src/Etnia_funcao.xlsx", 
                           col_types = c("date", "text", "text", 
                                         "text", "numeric"))
Etnia_funcao <- Etnia_funcao |> 
  tidyr::pivot_wider(names_from =Sexo, values_from = Total) |> 
  rowwise()  |> 
  mutate(
    Total = sum(c_across(Fem:Mas), na.rm = TRUE
    )
  )

graf <- Etnia_funcao |> 
  dplyr::select(-Fem, -Mas) |> 
  dplyr::filter(Data > "2017-12-01" ) |> 
  group_by(Data, Agrupamento2) |> 
  mutate(
    `% Cargo-Função/Etnia` = formattable::percent(Total/sum(Total, na.rm = TRUE))
  ) |> select(-Total) |> 
  tidyr::pivot_wider(names_from =Etnia,
                     values_from = `% Cargo-Função/Etnia`) |> 
  #dplyr::filter(Agrupamento2== "FCPE & FEX") |> 
  dplyr::ungroup() 

# Salvar base tratada
saveRDS(graf, "data/serie.rds")

sd_total <- SharedData$new(graf_fem, group = "dados_subset")  
filtro_funcao <- filter_select("Funcao", 
                               "Selecione um Cargo-Função", 
                               sd_total, 
                               ~Agrupamento2,
                               multiple = FALSE)

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE
)


fig1 <- plot_ly(sd_total, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Data, y = ~BRANCA, name = ~Agrupamento2)%>%
  layout(legend=list(title=list(text='Cor/Raça-Etnia')), xaxis = ax, yaxis = list(range = c(0.0,.8), title = ''))


fig2 <- plot_ly(sd_total, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Data, y = ~PARDA, name = ~Agrupamento2)%>%
  layout(legend=list(title=list(text='Cor/Raça-Etnia')), xaxis = ax, yaxis = list(range = c(0.0,0.8),title = '', showticklabels = TRUE))


fig3 <- plot_ly(sd_total, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Data, y = ~PRETA, name = ~Agrupamento2)%>%
  layout(legend=list(title=list(text='Cor/Raça-Etnia')), xaxis = ax, yaxis = list(range = c(0.0,0.3), title = 'Decreto'))


fig4 <- plot_ly(sd_total, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Data, y = ~AMARELA, name = ~Agrupamento2)%>%
  layout(legend=list(title=list(text='Cor/Raça-Etnia')), xaxis = ax, yaxis = list(range = c(0.0,0.3),title = '', showticklabels = TRUE))


fig5 <- plot_ly(sd_total, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Data, y = ~INDIGENA, name = ~Agrupamento2)%>%
  layout(legend=list(title=list(text='Cor/Raça-Etnia')),  yaxis = list(range = c(0.0,0.1),title = '', showticklabels = TRUE), xaxis = list(title = 'Data'))


fig6 <- plot_ly(sd_total, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Data, y = ~`NAO INFORMADO`, name = ~Agrupamento2)%>%
  layout( legend=list(title=list(text='Cor/Raça-Etnia')), yaxis = list(range = c(0.0,0.1) ,showticklabels = TRUE, title =''),  xaxis = list(title = 'Data'))


fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6,
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
    text = "Branca",
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
    text = "Parda",
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
    text = "Preta",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.775,
    y = 0.64,
    font = list(size = 10),
    text = "Amarela",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.225,
    y = 0.315,
    font = list(size = 10),
    text = "Indígena",
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
    text = "Não Informado",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
)

fig <- fig %>%layout(annotations = annotations, width = 1000)|> hide_legend()
options(warn = -1)
fig

bscols(
  filtro_funcao,
  fig,
  widths = c(12, 12))



# De Para - Funções -------------------------------------------------------


DE_PARA_FUNCOES <- read_excel("src/DE-PARA_FUNCOES.xlsx")|> janitor::clean_names()

funcoes_mar_23 |> left_join(DE_PARA_FUNCOES, by = c("Nível Função2" = "funcao")) 

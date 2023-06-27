library(readxl)
library(tidyverse)
library(reactablefmtr)
library(sysfonts)
library(showtext)
library(htmltools)

library(htmlwidgets)

#Filtro base Funções.qvd mes de março

funcoes <- readr::read_delim("src/arquivo.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
                             

# Etnia = Negras(Preta +Pardo) e demais

funcoes <- funcoes |> mutate(
  Etnia = case_when( 
    `Cor Origem Etnica` %in% c(4, 6)  ~ "Negras",
    .default = "Demais Raça/Cor"
  ),
  month = format(as.Date(paste("2023", `Mês Cargos`, '01', sep = '-'),"%Y-%b-%d"))
) |> janitor::clean_names()



# tratamento da base

tabela_org_sup <- funcoes |> group_by(
  month,
  mes_cargos,
  orgao_superior_cargos_e_funcoes, 
  #orgao_vinculado_cargos_e_funcoes,
  etnia , decreto_nivel) |> 
  dplyr::summarise(
    total = sum(quantidade_de_vinculos_cargos_e_funcoes)
  ) |> ungroup() |> 
  rename(
    `Órgão Superior` = orgao_superior_cargos_e_funcoes,
    #`Órgão` = orgao_vinculado_cargos_e_funcoes,
    `Cargo-Função` = decreto_nivel
  ) |> 
  filter(`Cargo-Função` %in% c("Nível 1 a 12", "Nível 13 a 17")) |> 
  tidyr::pivot_wider(names_from =`Cargo-Função`,
                     values_from = total) |> 
  arrange(month)
 

# Percentual das funcoes por etnia

tabela_org_sup_perc <- tabela_org_sup |>
  filter(mes_cargos == "Mai") |> 
  group_by(month, mes_cargos,`Órgão Superior` ) |> 
  mutate(
    `% Nível 1 a 12` = round(`Nível 1 a 12`/sum(`Nível 1 a 12`, na.rm = TRUE)*100, 1),
    `% Nível 13 a 17` = round(`Nível 13 a 17`/sum(`Nível 13 a 17`, na.rm = TRUE)*100, 1),
    #Total = sum(c_across(`Nível 1 a 12`:`Nível 13 a 17`), na.rm = TRUE)
  ) |>  
  arrange(-`% Nível 1 a 12`,-`% Nível 13 a 17` ) |> 
  ungroup() |> 
  group_by(month, mes_cargos,`Órgão Superior`) |> 
  mutate(
    Total_serv_1a12 = sum(sum(`Nível 1 a 12`, na.rm = TRUE)), 
    Total_serv_13a17 = sum(sum(`Nível 13 a 17`, na.rm = TRUE)), 
    ) |>  
  filter(etnia == "Negras")|> 
  ungroup() |> 
  mutate(rank=row_number())|>
  select(rank, `Órgão Superior`, `% Nível 1 a 12`, Total_serv_1a12 , `% Nível 13 a 17`, Total_serv_13a17)

# tendendia percentual
 

tend_mensal <- tabela_org_sup |>
    group_by(month, mes_cargos,`Órgão Superior` ) |> 
  mutate(
    `% Nível 1 a 12` = round(`Nível 1 a 12`/sum(`Nível 1 a 12`, na.rm = TRUE)*100, 1),
    `% Nível 13 a 17` = round(`Nível 13 a 17`/sum(`Nível 13 a 17`, na.rm = TRUE)*100, 1),
  ) |> 
  filter( etnia == "Negras") |> 
  ungroup() |> 
  group_by(`Órgão Superior`)|>  
  summarise(
    tendencia_1a12 = list(`% Nível 1 a 12`), 
    tendencia_13a17 = list(`% Nível 13 a 17`), .groups = "drop"
    )
               
               
data_1a12 <-  tabela_org_sup_perc |> 
  left_join(tend_mensal, by = join_by(`Órgão Superior`)) |> 
  mutate(Órgão = "") |> 
  select(rank, `Órgão Superior`, Órgão , Total_serv_1a12, `% Nível 1 a 12`, tendencia_1a12,
        # Total_serv_13a17, `% Nível 13 a 17`, tendencia_13a17
         )
# Salvar base tratada
saveRDS(data_1a12, "data/data_1a12.rds")               

# subdata -----------------------------------------------------------------

tabela_org <- funcoes |> group_by(
  month,
  mes_cargos,
  orgao_superior_cargos_e_funcoes, 
  orgao_vinculado_cargos_e_funcoes,
  etnia , decreto_nivel) |> 
  dplyr::summarise(
    total = sum(quantidade_de_vinculos_cargos_e_funcoes)
  ) |> ungroup() |> 
  rename(
    `Órgão Superior` = orgao_superior_cargos_e_funcoes,
    `Órgão` = orgao_vinculado_cargos_e_funcoes,
    `Cargo-Função` = decreto_nivel
  ) |> 
  filter(`Cargo-Função` %in% c("Nível 1 a 12", "Nível 13 a 17")) |> 
  tidyr::pivot_wider(names_from =`Cargo-Função`,
                     values_from = total) |> 
  arrange(month)

# Percentual das funcoes por etnia

tabela_org_perc <- tabela_org |>
  filter(mes_cargos == "Mai") |> 
  group_by(month, mes_cargos,`Órgão Superior`, Órgão ) |> 
  mutate(
    `% Nível 1 a 12` = round(`Nível 1 a 12`/sum(`Nível 1 a 12`, na.rm = TRUE)*100, 1),
    `% Nível 13 a 17` = round(`Nível 13 a 17`/sum(`Nível 13 a 17`, na.rm = TRUE)*100, 1),
    #Total = sum(c_across(`Nível 1 a 12`:`Nível 13 a 17`), na.rm = TRUE)
  ) |>  
  arrange(-`% Nível 1 a 12`,-`% Nível 13 a 17` ) |> 
  ungroup() |> 
  group_by(month, mes_cargos,`Órgão Superior`, Órgão) |> 
  mutate(
    Total_serv_1a12 = sum(sum(`Nível 1 a 12`, na.rm = TRUE)), 
    Total_serv_13a17 = sum(sum(`Nível 13 a 17`, na.rm = TRUE)), 
  ) |>  
  filter(etnia == "Negras")|> 
  ungroup() |> 
  mutate(rank=row_number())|>
  select(rank, `Órgão Superior`, Órgão, `% Nível 1 a 12`, Total_serv_1a12 , `% Nível 13 a 17`, Total_serv_13a17)

tend_mensal_org <- tabela_org |>
  group_by(month, mes_cargos,`Órgão Superior` , Órgão) |> 
  mutate(
    `% Nível 1 a 12` = round(`Nível 1 a 12`/sum(`Nível 1 a 12`, na.rm = TRUE)*100, 1),
    `% Nível 13 a 17` = round(`Nível 13 a 17`/sum(`Nível 13 a 17`, na.rm = TRUE)*100, 1),
  ) |> 
  filter( etnia == "Negras") |> 
  ungroup() |> 
  group_by(`Órgão Superior`, Órgão)|>  
  summarise(
    tendencia_1a12 = list(`% Nível 1 a 12`), 
    tendencia_13a17 = list(`% Nível 13 a 17`), .groups = "drop"
  )

subdata1a12 <-  tabela_org_perc |> 
  left_join(tend_mensal_org, by = join_by(`Órgão Superior`, Órgão)) |> 
  select(rank, `Órgão Superior`, Órgão , Total_serv_1a12, `% Nível 1 a 12`, tendencia_1a12,
        # Total_serv_13a17, `% Nível 13 a 17`, tendencia_13a17
         )

# Salvar base tratada
saveRDS(subdata1a12, "data/subdata1a12.rds")

# Rectable 1a 12 ----------------------------------------------------------

       
#color palette for difficulty scale
#text_color="#555555",
pal_strive<-c('#50C4AA', '#B6C95C', '#FACB3E', '#FC800F', '#FF4759')



# Nivel 1 a 12 ------------------------------------------------------------


#Create Reactable
table<-reactable(
  data,
  theme =  default(),
  #   reactableTheme(
  #   style=list(fontFamily="Roboto"),
  #   searchInputStyle = list(background="black"),
  #   pageButtonStyle = list(fontSize=14),
  #   backgroundColor="black",
  #   color="white",
  #   footerStyle = list(color="white", fontSize=11),
  #   borderColor="#3D3D3D",
  #   borderWidth=0.019
  # ),
  defaultColDef = colDef(vAlign="center", align="center", headerVAlign="center"),
  columns = list(
    #image_path = colDef(show=FALSE),
    `Órgão Superior` = colDef(name="Órgão Superior", 
                              align="left", vAlign="center", width=220,
                              # resizable = TRUE,sortable = TRUE,
                              # filterable = TRUE,
                              cell = function(value) {
                                #image <- img(src = paste0(base_image_url,str_replace_all(value," ","_"),".png"), style = "height: 33px;", alt = value)
                                tagList(
                                  div(style = "display: inline-block;vertical-align:middle;width:50px"),
                                  div(style = "display: inline-block;vertical-align:middle;", value)
                                )
                              }
                              ),
    Órgão = colDef(name="Órgão", maxWidth=130, align="left"),
    rank = colDef(name="", style=list(fontSize=13), maxWidth=50, align="right"),
    Total_serv_1a12 = colDef(name="Nº de Cargos/Funções", minWidth=50),
    `% Nível 1 a 12` =colDef(name="%", minWidth=120, 
                   cell=data_bars(data, 
                                  bar_height=8,
                                  text_size=11,
                                  text_color="rgb(51, 51, 51)",
                                  text_position = "outside-end", 
                                  background = "transparent", 
                                  round_edges = TRUE, 
                                  fill_color=c("#FFBC51",'#FF3A3A'), 
                                  fill_gradient = TRUE)),
    # difficulty = colDef(name="AVG DIFF", align="center", maxWidth=120, 
    #                     footer="Weighted avg by minutes",
    #                     cell=color_tiles(data, bias= 0.4, colors=pal_strive)),
    tendencia_1a12 = colDef(name="Tendência %",  maxWidth=120,
                          footer="% Últimos 5 meses",
                          cell=react_sparkline(data, labels=c("first","last"), 
                                               tooltip_size = "1.1em",
                                               tooltip_type=1,
                                               line_color = "rgb(51, 51, 51)")
    )
    # Total_serv_13a17 = colDef(name="Nº de Cargos Comis - Nível 1 a 12", minWidth=120),
    # `% Nível 13 a 17` =colDef(name="% Negros", minWidth=160, 
    #                          cell=data_bars(data, 
    #                                         bar_height=8,
    #                                         text_size=11,
    #                                         text_color="white",
    #                                         text_position = "outside-end", 
    #                                         background = "transparent", 
    #                                         round_edges = TRUE, 
    #                                         fill_color=c("#FFBC51",'#FF3A3A'), 
    #                                         fill_gradient = TRUE)),
    # # difficulty = colDef(name="AVG DIFF", align="center", maxWidth=120, 
    # #                     footer="Weighted avg by minutes",
    # #                     cell=color_tiles(data, bias= 0.4, colors=pal_strive)),
    # tendencia_13a17 = colDef(name="Tendência % - Nivel 13 a 17",  maxWidth=100,
    #                         footer="% mensal",
    #                         cell=react_sparkline(data, labels=c("first","last"), 
    #                                              tooltip_size = "1.1em",
    #                                              tooltip_type=1,
    #                                              line_color = "white")
    # )
    ),
  #Sub-Table - nested reactable, when user clicks on instructor, details show aggregates by modal per instructor
  details = function(index){
    new = subdata[subdata$`Órgão Superior` == data$`Órgão Superior`[index],]
    reactable(data=new,
              defaultColDef = colDef(vAlign="center", align="center", headerVAlign="center"),
              theme =  default(),
              #   reactableTheme(
              #   style=list(fontFamily="Roboto"),
              #   searchInputStyle = list(background="black"),
              #   pageButtonStyle = list(fontSize=14),
              #   backgroundColor="black",
              #   color="white",
              #   footerStyle = list(color="white", fontSize=11),
              #   borderColor="black",
              #   borderWidth=0.019
              # ),
              columns = list(
                `Órgão Superior`=colDef(show=FALSE),
                rank = colDef(name="", width=295),
                Órgão = colDef(name="", maxWidth=130, align="left", footer="", footerStyle=list(color='black')), #
                rank = colDef(name="", style=list(fontSize=13), maxWidth=50, align="right"),
                Total_serv_1a12 = colDef(name="", minWidth=50),
                `% Nível 1 a 12` = colDef(name="", minWidth=120, 
                               cell=data_bars(new, 
                                              bar_height=8,
                                              text_size=11,
                                              text_color="rgb(51, 51, 51)",
                                              text_position = "outside-end", 
                                              background = "transparent", 
                                              round_edges = TRUE, 
                                              fill_color=c("#FFBC51",'#FF3A3A'), 
                                              fill_gradient = TRUE)),
                # difficulty = colDef(name="", align="center", maxWidth=120, 
                #                     cell=color_tiles(new, bias= 0.4, colors=pal_strive)),
                tendencia_1a12 = colDef(name="",  maxWidth=120,
                                      cell=react_sparkline(new, labels=c("first","last"), 
                                                           line_color = "rgb(51, 51, 51)")
                )
                # Total_serv_13a17 = colDef(name="", minWidth=120),
                # `% Nível 13 a 17` = colDef(name="", minWidth=160, 
                #                           cell=data_bars(new, 
                #                                          bar_height=8,
                #                                          text_size=11,
                #                                          text_color="white",
                #                                          text_position = "outside-end", 
                #                                          background = "transparent", 
                #                                          round_edges = TRUE, 
                #                                          fill_color=c("#FFBC51",'#FF3A3A'), 
                #                                          fill_gradient = TRUE)),
                # # difficulty = colDef(name="", align="center", maxWidth=120, 
                # #                     cell=color_tiles(new, bias= 0.4, colors=pal_strive)),
                # tendencia_13a17 = colDef(name="",  maxWidth=100,
                #                         cell=react_sparkline(new, labels=c("first","last"), 
                #                                              line_color = "white")
                # )
                )
    )
  }
)%>%
  google_font(font_family="Roboto", font_weight = 300)



#use html widgest to prepend an dappend header and footer
html_object<-table|>
  prependContent(
    tagList(
      div(style = "vertical-align:middle;text-align:center;background-color:#1351B4;color:white;padding-top:25px;padding-bottom:4px;font-size:24px;",
          "Percentual de Pessoas Negras (Pretas/Pardas) em Cargos Comissionados de Níveis de 1 a 12"),
      div(style = "vertical-align:middle;text-align:center;background-color:#1351B4;color:#BBBBBB;padding-top:5px;padding-bottom:20px;font-size:14px;",
          "Decreto nº 11.443/2023 que reserva às pessoas negras (pretas e pardas) percentual mínimo de 30% na ocupação em Cargos Comissionados Executivos (CCE) e Funções Comissionadas Executivas (FCE)")
    )
  )|>
  appendContent(
    p("Fonte: Painel Estatístico de Pessoal",
      style = paste0(
        "font-family:","Roboto; sans;",
        "font-size: 12px;" ,
        "text-align:right"))
  )


saveWidget(html_object, "nivel1a12.html", selfcontained = TRUE)



# Nivel 13 a 17 -----------------------------------------------------------


# Percentual das funcoes por etnia

tabela_org_sup_perc <- tabela_org_sup |>
  filter(mes_cargos == "Mai") |> 
  group_by(month, mes_cargos,`Órgão Superior` ) |> 
  mutate(
    `% Nível 1 a 12` = round(`Nível 1 a 12`/sum(`Nível 1 a 12`, na.rm = TRUE)*100, 1),
    `% Nível 13 a 17` = round(`Nível 13 a 17`/sum(`Nível 13 a 17`, na.rm = TRUE)*100, 1),
    #Total = sum(c_across(`Nível 1 a 12`:`Nível 13 a 17`), na.rm = TRUE)
  ) |>  
  arrange(-`% Nível 13 a 17`, -`% Nível 1 a 12`) |> 
  ungroup() |> 
  group_by(month, mes_cargos,`Órgão Superior`) |> 
  mutate(
    Total_serv_1a12 = sum(sum(`Nível 1 a 12`, na.rm = TRUE)), 
    Total_serv_13a17 = sum(sum(`Nível 13 a 17`, na.rm = TRUE)), 
  ) |>  
  filter(etnia == "Negras")|> 
  ungroup() |> 
  mutate(rank=row_number())|>
  select(rank, `Órgão Superior`, `% Nível 1 a 12`, Total_serv_1a12 , `% Nível 13 a 17`, Total_serv_13a17)

# tendendia percentual


tend_mensal <- tabela_org_sup |>
  group_by(month, mes_cargos,`Órgão Superior` ) |> 
  mutate(
    `% Nível 1 a 12` = round(`Nível 1 a 12`/sum(`Nível 1 a 12`, na.rm = TRUE)*100, 1),
    `% Nível 13 a 17` = round(`Nível 13 a 17`/sum(`Nível 13 a 17`, na.rm = TRUE)*100, 1),
  ) |> 
  filter( etnia == "Negras") |> 
  ungroup() |> 
  group_by(`Órgão Superior`)|>  
  summarise(
    tendencia_1a12 = list(`% Nível 1 a 12`), 
    tendencia_13a17 = list(`% Nível 13 a 17`), .groups = "drop"
  )


data_13a17 <-  tabela_org_sup_perc |> 
  left_join(tend_mensal, by = join_by(`Órgão Superior`)) |> 
  mutate(Órgão = "") |> 
  select(rank, `Órgão Superior`, Órgão , Total_serv_13a17, `% Nível 13 a 17`, tendencia_13a17  )

# Salvar base tratada
saveRDS(data_13a17, "data/data_13a17.rds")

# Percentual das funcoes por etnia

tabela_org_perc <- tabela_org |>
  filter(mes_cargos == "Mai") |> 
  group_by(month, mes_cargos,`Órgão Superior`, Órgão ) |> 
  mutate(
    `% Nível 1 a 12` = round(`Nível 1 a 12`/sum(`Nível 1 a 12`, na.rm = TRUE)*100, 1),
    `% Nível 13 a 17` = round(`Nível 13 a 17`/sum(`Nível 13 a 17`, na.rm = TRUE)*100, 1),
    #Total = sum(c_across(`Nível 1 a 12`:`Nível 13 a 17`), na.rm = TRUE)
  ) |>  
  arrange(-`% Nível 13 a 17`,-`% Nível 1 a 12` ) |> 
  ungroup() |> 
  group_by(month, mes_cargos,`Órgão Superior`, Órgão) |> 
  mutate(
    Total_serv_1a12 = sum(sum(`Nível 1 a 12`, na.rm = TRUE)), 
    Total_serv_13a17 = sum(sum(`Nível 13 a 17`, na.rm = TRUE)), 
  ) |>  
  filter(etnia == "Negras")|> 
  ungroup() |> 
  mutate(rank=row_number())|>
  select(rank, `Órgão Superior`, Órgão, `% Nível 1 a 12`, Total_serv_1a12 , `% Nível 13 a 17`, Total_serv_13a17)

tend_mensal_org <- tabela_org |>
  group_by(month, mes_cargos,`Órgão Superior` , Órgão) |> 
  mutate(
    `% Nível 1 a 12` = round(`Nível 1 a 12`/sum(`Nível 1 a 12`, na.rm = TRUE)*100, 1),
    `% Nível 13 a 17` = round(`Nível 13 a 17`/sum(`Nível 13 a 17`, na.rm = TRUE)*100, 1),
  ) |> 
  filter( etnia == "Negras") |> 
  ungroup() |> 
  group_by(`Órgão Superior`, Órgão)|>  
  summarise(
    tendencia_1a12 = list(`% Nível 1 a 12`), 
    tendencia_13a17 = list(`% Nível 13 a 17`), .groups = "drop"
  )

subdata_13a17 <-  tabela_org_perc |> 
  left_join(tend_mensal_org, by = join_by(`Órgão Superior`, Órgão)) |> 
  select(rank, `Órgão Superior`, Órgão , Total_serv_13a17, `% Nível 13 a 17`, tendencia_13a17)


# Salvar base tratada
saveRDS(subdata_13a17, "data/subdata_13a17.rds")


# Reactable 13 a 17 -------------------------------------------------------



#color palette for difficulty scale
#text_color="#555555",
pal_strive<-c('#50C4AA', '#B6C95C', '#FACB3E', '#FC800F', '#FF4759')



# Nivel 13 a 17 ------------------------------------------------------------


#Create Reactable
table<-reactable(
  data,
  theme =  default(),
  #   reactableTheme(
  #   style=list(fontFamily="Roboto"),
  #   searchInputStyle = list(background="black"),
  #   pageButtonStyle = list(fontSize=14),
  #   backgroundColor="black",
  #   color="white",
  #   footerStyle = list(color="white", fontSize=11),
  #   borderColor="#3D3D3D",
  #   borderWidth=0.019
  # ),
  defaultColDef = colDef(vAlign="center", align="center", headerVAlign="center"),
  columns = list(
    #image_path = colDef(show=FALSE),
    `Órgão Superior` = colDef(name="Órgão Superior", 
                              align="left", vAlign="center", width=220,
                              # resizable = TRUE,sortable = TRUE,
                              # filterable = TRUE,
                              cell = function(value) {
                                #image <- img(src = paste0(base_image_url,str_replace_all(value," ","_"),".png"), style = "height: 33px;", alt = value)
                                tagList(
                                  div(style = "display: inline-block;vertical-align:middle;width:50px"),
                                  div(style = "display: inline-block;vertical-align:middle;", value)
                                )
                              }
    ),
    Órgão = colDef(name="Órgão", maxWidth=130, align="left"),
    rank = colDef(name="", style=list(fontSize=13), maxWidth=50, align="right"),
    Total_serv_13a17 = colDef(name="Nº de Cargos/Funções", minWidth=50),
    `% Nível 13 a 17` =colDef(name="%", minWidth=120, 
                             cell=data_bars(data, 
                                            bar_height=8,
                                            text_size=11,
                                            text_color="rgb(51, 51, 51)",
                                            text_position = "outside-end", 
                                            background = "transparent", 
                                            round_edges = TRUE, 
                                            fill_color=c("#FFBC51",'#FF3A3A'), 
                                            fill_gradient = TRUE)),
    # difficulty = colDef(name="AVG DIFF", align="center", maxWidth=120, 
    #                     footer="Weighted avg by minutes",
    #                     cell=color_tiles(data, bias= 0.4, colors=pal_strive)),
    tendencia_13a17 = colDef(name="Tendência %",  maxWidth=120,
                            footer="% Últimos 5 meses",
                            cell=react_sparkline(data, labels=c("first","last"), 
                                                 tooltip_size = "1.1em",
                                                 tooltip_type=1,
                                                 line_color = "rgb(51, 51, 51)")
    )
    # Total_serv_13a17 = colDef(name="Nº de Cargos Comis - Nível 1 a 12", minWidth=120),
    # `% Nível 13 a 17` =colDef(name="% Negros", minWidth=160, 
    #                          cell=data_bars(data, 
    #                                         bar_height=8,
    #                                         text_size=11,
    #                                         text_color="white",
    #                                         text_position = "outside-end", 
    #                                         background = "transparent", 
    #                                         round_edges = TRUE, 
    #                                         fill_color=c("#FFBC51",'#FF3A3A'), 
    #                                         fill_gradient = TRUE)),
    # # difficulty = colDef(name="AVG DIFF", align="center", maxWidth=120, 
    # #                     footer="Weighted avg by minutes",
    # #                     cell=color_tiles(data, bias= 0.4, colors=pal_strive)),
    # tendencia_13a17 = colDef(name="Tendência % - Nivel 13 a 17",  maxWidth=100,
    #                         footer="% mensal",
    #                         cell=react_sparkline(data, labels=c("first","last"), 
    #                                              tooltip_size = "1.1em",
    #                                              tooltip_type=1,
    #                                              line_color = "white")
    # )
  ),
  #Sub-Table - nested reactable, when user clicks on instructor, details show aggregates by modal per instructor
  details = function(index){
    new = subdata[subdata$`Órgão Superior` == data$`Órgão Superior`[index],]
    reactable(data=new,
              defaultColDef = colDef(vAlign="center", align="center", headerVAlign="center"),
              theme =  default(),
              #   reactableTheme(
              #   style=list(fontFamily="Roboto"),
              #   searchInputStyle = list(background="black"),
              #   pageButtonStyle = list(fontSize=14),
              #   backgroundColor="black",
              #   color="white",
              #   footerStyle = list(color="white", fontSize=11),
              #   borderColor="black",
              #   borderWidth=0.019
              # ),
              columns = list(
                `Órgão Superior`=colDef(show=FALSE),
                rank = colDef(name="", width=295),
                Órgão = colDef(name="", maxWidth=130, align="left", footer="", footerStyle=list(color='black')), #
                rank = colDef(name="", style=list(fontSize=13), maxWidth=50, align="right"),
                Total_serv_13a17 = colDef(name="", minWidth=50),
                `% Nível 13 a 17` = colDef(name="", minWidth=120, 
                                          cell=data_bars(new, 
                                                         bar_height=8,
                                                         text_size=11,
                                                         text_color="rgb(51, 51, 51)",
                                                         text_position = "outside-end", 
                                                         background = "transparent", 
                                                         round_edges = TRUE, 
                                                         fill_color=c("#FFBC51",'#FF3A3A'), 
                                                         fill_gradient = TRUE)),
                # difficulty = colDef(name="", align="center", maxWidth=120, 
                #                     cell=color_tiles(new, bias= 0.4, colors=pal_strive)),
                tendencia_13a17 = colDef(name="",  maxWidth=120,
                                        cell=react_sparkline(new, labels=c("first","last"), 
                                                             line_color = "rgb(51, 51, 51)")
                )
                # Total_serv_13a17 = colDef(name="", minWidth=120),
                # `% Nível 13 a 17` = colDef(name="", minWidth=160, 
                #                           cell=data_bars(new, 
                #                                          bar_height=8,
                #                                          text_size=11,
                #                                          text_color="white",
                #                                          text_position = "outside-end", 
                #                                          background = "transparent", 
                #                                          round_edges = TRUE, 
                #                                          fill_color=c("#FFBC51",'#FF3A3A'), 
                #                                          fill_gradient = TRUE)),
                # # difficulty = colDef(name="", align="center", maxWidth=120, 
                # #                     cell=color_tiles(new, bias= 0.4, colors=pal_strive)),
                # tendencia_13a17 = colDef(name="",  maxWidth=100,
                #                         cell=react_sparkline(new, labels=c("first","last"), 
                #                                              line_color = "white")
                # )
              )
    )
  }
)%>%
  google_font(font_family="Roboto", font_weight = 300)



#use html widgest to prepend an dappend header and footer
html_object<-table|>
  prependContent(
    tagList(
      div(style = "vertical-align:middle;text-align:center;background-color:#1351B4;color:white;padding-top:25px;padding-bottom:4px;font-size:24px;",
          "Percentual de Pessoas Negras (Pretas/Pardas) em Cargos Comissionados de Níveis de 13 a 17"),
      div(style = "vertical-align:middle;text-align:center;background-color:#1351B4;color:#BBBBBB;padding-top:5px;padding-bottom:20px;font-size:14px;",
          "Decreto nº 11.443/2023 que reserva às pessoas negras (pretas e pardas) percentual mínimo de 30% na ocupação em Cargos Comissionados Executivos (CCE) e Funções Comissionadas Executivas (FCE)")
    )
  )|>
  appendContent(
    p("Fonte: Painel Estatístico de Pessoal",
      style = paste0(
        "font-family:","Roboto; sans;",
        "font-size: 12px;" ,
        "text-align:right"))
  )


saveWidget(html_object, "nivel13a17.html", selfcontained = TRUE)

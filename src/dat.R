library(dplyr)
library(readxl)

#Filtro base Funções.qvd mes de março

funcoes_mar_23 <- read_excel("src/funcoes_mar_23.xlsx", 
                             col_types = c("text", "numeric", "numeric", 
                                           "text", "numeric", "numeric", "numeric", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "text", "numeric", 
                                           "text", "text", "text", "numeric", 
                                           "numeric", "text", "date", "text", 
                                           "text", "text", "text", "text", "numeric", 
                                           "text", "text", "text", "text"))

# tratamento da base

tabela <- funcoes_mar_23 |> group_by(
  `Orgão Vinculado (Cargos e Funçõe`,
  `Nome Cor Origem Etnica`,Sexo, Agrupamento2) |> 
  dplyr::summarise(
    total = sum(`Quantidade de Vinculos (Cargos e`)
  ) |> ungroup() |> 
  rename(
    `Orgão` = `Orgão Vinculado (Cargos e Funçõe`,
    Etnia = `Nome Cor Origem Etnica`, 
    `Cargo-Função` = Agrupamento2
  ) |> 
  tidyr::pivot_wider(names_from =Sexo,
                     values_from = total) |> 
  mutate(
    Total = Fem + Mas, 
    Etnia = factor(Etnia, 
                   levels = c(
                     "BRANCA","PARDA", "PRETA", "AMARELA","INDIGENA", "NAO INFORMADO")
    )
    #dados_id = dplyr::row_number()
  )

# base para gerar mapas
#DADOS_LAT_LONG_ESTADOS <- read_excel("src/DADOS_LAT_LONG_ESTADOS.xlsx")

#Tab <- tabela |> left_join(DADOS_LAT_LONG_ESTADOS, 
#                           join_by(`UF da Organização` == UF) ) 

# Salvar base tratada
saveRDS(tabela, "data/Tab.rds")

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


"#FFF7FB" "#ECE7F2" "#D0D1E6" "#A6BDDB" "#74A9CF" "#3690C0" "#0570B0" "#034E7B"
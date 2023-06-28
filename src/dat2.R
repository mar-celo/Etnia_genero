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



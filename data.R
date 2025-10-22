

## code to prepare `DATASET` dataset goes here
install.packages(c("renv","remotes", "dplyr", "lubridate", "janitor", "readr", "echarts4r", "htmltools", "stringr" ))

pacotes <- renv::dependencies() |>
  dplyr::filter(!Package %in% c("renv", "dplyr")) |>
  dplyr::pull(Package) |>
  unique()

install.packages(pacotes)
remotes::install_github("timelyportfolio/dataui")

library(janitor)
library(dplyr)
library(readr)
library(echarts4r)
library(htmltools)


# Carregue a biblioteca lubridate
library(lubridate)

locale <- Sys.getlocale("LC_TIME")

# Defina o local para o Brasil

Sys.setlocale("LC_TIME", "pt_BR.ISO8859-1")

# Obtenha a data atual
data_atual <- Sys.Date()


# Subtrai um mês da data atual
data_mes_anterior <- data_atual %m-% months(1)

# Formate o mês como três letras iniciais com a primeira letra em maiúscula
mes_anterior_abreviado <- format(data_mes_anterior, "%b") |> stringr::str_to_title()



# Carregar base de dados direto do PEP

df <- readr::read_delim("Y:/PEP/PEP_reload/PEP_qvd_InOutrasFontes/Fontes_CSV/Infograficos/etnia_raca.csv",
     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Carregar base de dados direto do repositorio

#df <- readr::read_csv("data/etnia_raca.csv")

# Conferencia com o PEP

df_conf <- df |>
  filter(`Agrupamento Geral` == 'CCE & FCE',
         `Mês Cargos` == mes_anterior_abreviado) |>
  # dplyr::summarise(
  #          total = sum(`Quantidade de Vinculos (Cargos e Funções)`)
  #        ) |> 
  group_by(
    # orgao_superior_cargos_e_funcoes,
    # orgao_vinculado_cargos_e_funcoes,
    # `Nome Cor Origem Etnica`,
    # sexo,
    `Decreto Nivel`
  ) |>
  dplyr::summarise(
    total = sum(`Quantidade de Vinculos (Cargos e Funções)`)
  ) |>
  ungroup() |>
  filter(!`Decreto Nivel` %in% c("Nível 18"))
                             

# Etnia = Negras(Preta +Pardo) e demais

df <- df |>  
  filter(`Agrupamento Geral` == 'CCE & FCE', 
         `Mês Cargos` == mes_anterior_abreviado)  |> 
  #filter( stringr::str_detect(df$`Nível Função2`, '^(FCE)|(CCE)'))  |>
  mutate(
  Etnia = case_when( 
    `Cor Origem Etnica` %in% c(4, 6)  ~ "Negras",
    .default = "Demais Raça/Cor"
  )
) |> janitor::clean_names()






# tratamento da base

tabela <- df |> 
 group_by(
 orgao_superior_cargos_e_funcoes,
 orgao_vinculado_cargos_e_funcoes,
  etnia ,
  sexo,
 decreto_nivel
 ) |>
  dplyr::summarise(
    total = sum(quantidade_de_vinculos_cargos_e_funcoes)
  ) |> 
  ungroup() |>  
  filter(!decreto_nivel %in% c("Nível 18")) |> 
  # janitor::adorn_totals()
  rename(
    `Órgão Superior` = orgao_superior_cargos_e_funcoes,
    `Órgão` = orgao_vinculado_cargos_e_funcoes,
    `Cargo-Função` = decreto_nivel
  ) |> 
  tidyr::pivot_wider(
    names_from =sexo,
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
  group_by(`Órgão`,  `Cargo-Função`) |> 
  mutate(
    `% Cargo-Função/Etnia` = scales::percent(Total/sum(Total, na.rm = TRUE))
  ) |>  ungroup()



# Salvar base tratada
saveRDS(tabela, "data/Tab.rds")





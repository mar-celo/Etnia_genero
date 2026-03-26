

## code to prepare `DATASET` dataset goes here
install.packages(c("renv","remotes", "dplyr", "lubridate", "janitor", "readr",
                   "echarts4r", "htmltools", "stringr" ,"zoo"))

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
library(zoo)
library(stringr)



limpa_string <- function(x){
  x %>% 
    tolower() %>% 
    iconv("utf-8", "ASCII//TRANSLIT") %>% 
    str_squish()
}


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
ano_corrente <- year(data_mes_anterior)


# Carregar base de dados direto do PEP
# pep_folder <- "Y:/PEP/PEP_reload/PEP_qvd_InOutrasFontes/Infograficos/
pep_folder <- "C:/Users/wesley.jesus/Documents/Monitoramento_politicas_SGP/monitoramento_cotas"

df <- readr::read_delim(file.path(pep_folder, "/etnia_raca.csv"),
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)



# Carregar base de dados direto do repositorio

#df <- readr::read_csv("data/etnia_raca.csv")

# Conferencia com o PEP

df_conf <- etnia_raca |>
  # filter(#`Agrupamento Geral` == 'CCE & FCE' ,
  #        `Mês Cargos` == mes_anterior_abreviado,
  #         `Ano Cargos` == ano_corrente
  #         ) |>
  # dplyr::summarise(
  #          total = sum(`Quantidade de Vinculos (Cargos e Funções)`)
  #        ) |> 
  group_by(
    # orgao_superior_cargos_e_funcoes,
    # orgao_vinculado_cargos_e_funcoes,
    # `Nome Cor Origem Etnica`,
    # sexo,
    `Mês-Ano Cargos`
    #`Decreto Nivel`
  ) |>
  dplyr::summarise(
    total = sum(`Quantidade de Vinculos (Cargos e Funções)`)
  ) |>
  ungroup() %>%
  mutate(ano_mes := as.yearmon(`Mês-Ano Cargos`, "%B %Y")) #|>
  # filter(!`Decreto Nivel` %in% c("Nível 18"))
                             

# Etnia = Negras(Preta +Pardo) e demais

df <- df  |> 
  #filter( stringr::str_detect(df$`Nível Função2`, '^(FCE)|(CCE)'))  |>
  mutate(
  Etnia = case_when( 
    `Cor Origem Etnica` %in% c(4, 6)  ~ "Negras",
    .default = "Demais Raça/Cor"
  )
) |> janitor::clean_names()






# tratamento da base

tabela <- df |>  
  filter(agrupamento_geral == 'CCE & FCE', 
         ano_cargos == 2026,
         mes_cargos == "Jan") |> 
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
  ) %>% #View
  tidyr::pivot_wider(
    names_from =sexo,
    values_from = total) |> 
  rowwise()  #%>% View
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



### Indicadores 1 e 2: % de negros (mensal) ----


### Indicador 3: necessidade de cargos vagos ----

cargos_disp <- readxl::read_excel("data/orgao_superior_cargos.xlsx") %>% setDT
cargos_disp[,`:=`(
  
  ## limpeza no nome do órgão
  orgao = limpa_string(`Órgão ou Entidade`),
  
  ## filtrando para níveis 1 a 12, 13 a 17 
  `Cargo-Função` =
    ifelse(
      as.numeric(`Nível do Cargo/Função`) %in% 1:12,
      "Nível 1 a 12",
      ifelse(
        as.numeric(`Nível do Cargo/Função`) %in% 13:17,
        "Nível 13 a 17",
        NA)
    )
)]


# total de cargos FCE e CCE vagos por órgão e nível
cargos_disp[!is.na(`Cargo-Função`),
            .(CCE = sum(as.numeric(CCE),na.rm = T),
              FCE = sum(as.numeric(FCE),na.rm = T)
            ),
            .(`Órgão ou Entidade`,orgao,`Cargo-Função`)] %>% 
  rowwise() %>% 
  mutate(
    total_dist = sum(c_across(CCE:FCE), na.rm = TRUE)
    ) %>% 
  janitor::clean_names()%>%
  setDT -> cargos_niveis_disp


# tratamento da base
tabela_vagos <- df |>  
  filter(agrupamento_geral == 'CCE & FCE', 
         ano_cargos == 2026,
         mes_cargos == "Jan") |> 
  group_by(
    orgao_superior_cargos_e_funcoes,
    orgao_vinculado_cargos_e_funcoes,
    etnia,
    decreto_nivel
  ) %>% 
  dplyr::summarise(
    total_negras = sum(quantidade_de_vinculos_cargos_e_funcoes)
  ) |> 
  ungroup() |>  
  filter(!decreto_nivel %in% c("Nível 18")) %>%
  setDT %>%
  .[,Total_ocupados := sum(total_negras),
    .(orgao_superior_cargos_e_funcoes,
      orgao_vinculado_cargos_e_funcoes,
      decreto_nivel)] %>% 
  filter(etnia == "Negras") %>%
  select(-etnia)

# cruzamento com cargos vagos
tabela_vagos[,`:=`(orgao_sup_clean = limpa_string(orgao_superior_cargos_e_funcoes),
                   orgao_vinc_clean = limpa_string(orgao_vinculado_cargos_e_funcoes)
                )]


tabela_vagos <-
  tabela_vagos %>%
  # left_join(cargos_niveis_disp[,.(orgao,
  #                                 cargo_funcao,
  #                                 total_dist)],
  #           by = c("orgao_sup_clean" = "orgao",
  #                  "decreto_nivel" = "cargo_funcao")) %>%
  left_join(cargos_niveis_disp[,.(orgao,
                                  cargo_funcao,
                                  total_dist)],
            by = c("orgao_vinc_clean" = "orgao",
                   "decreto_nivel" = "cargo_funcao")) %>%
  # .[,total_dist := ifelse(is.na(total_dist.x),total_dist.y,total_dist.x)] %>% 
  select(-orgao_sup_clean,-orgao_vinc_clean)#,
         # -total_dist.x,-total_dist.y) 

tabela_vagos[,`:=`( necessidade_vagas =  ceiling(Total_ocupados*0.3) - total_negras,
                    cargos_disponiveis = total_dist - Total_ocupados)]
tabela_vagos[,`:=`( indice_necessidade = (necessidade_vagas/(cargos_disponiveis + 1)) %>%
                      ifelse(.< 0,NA,.))]

tabela_vagos <- 
  select(tabela_vagos,
         orgao_superior_cargos_e_funcoes,
         orgao_vinculado_cargos_e_funcoes,
         decreto_nivel,
         Total_ocupados,
         total_negras,
         # total_dist,
         necessidade_vagas,
         cargos_disponiveis,
         indice_necessidade
  ) %>%
  filter(!is.na(indice_necessidade))

# Salvar base tratada
saveRDS(tabela_vagos, "data/Tab_ind3.rds")

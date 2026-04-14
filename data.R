

## code to prepare `DATASET` dataset goes here
install.packages(c("renv","remotes", "dplyr", "lubridate", "janitor", "readr",
                   "echarts4r", "htmltools", "stringr" ,"zoo","data.table",
                   "sparklyr","pysparklyr","reticulate","readxl","lubridate",
                   "fuzzyjoin"))

pacotes <- renv::dependencies() |>
  dplyr::filter(!Package %in% c("renv", "dplyr")) |>
  dplyr::pull(Package) |>
  unique()

install.packages(pacotes)
remotes::install_github("timelyportfolio/dataui")

library(janitor)
library(dplyr)
library(dbplyr)
library(data.table)
library(readr)
library(echarts4r)
library(htmltools)
library(zoo)
library(stringr)
library(sparklyr)
library(pysparklyr)
library(reticulate)
library(readxl)
library(lubridate)
library(fuzzyjoin)



limpa_string <- function(x){
  x %>% 
    tolower() %>% 
    iconv("utf-8", "ASCII//TRANSLIT") %>% 
    str_squish()
}

# função para captar necessidade para meta
nec_meta <- function(o,n,p = 0.3){
  n_new = (p*o - n)/(1-p)
  return(
    ifelse(n_new < 0,
           0,
           ifelse(n_new %% round(n_new,1) <= 1e-10,
                  round(n_new,0),
                  ceiling(n_new)
                  )
           )
    )
}
  


# Carregue a biblioteca lubridate

locale <- Sys.getlocale("LC_TIME")

# Defina o local para o Brasil

Sys.setlocale("LC_TIME", "pt_BR.ISO8859-1")

# Obtenha a data atual
data_atual <- Sys.Date()


# Subtrai um mês da data atual
data_mes_anterior <- data_atual %m-% months(2)

# Formate o mês como três letras iniciais com a primeira letra em maiúscula
mes_anterior_abreviado <- format(data_mes_anterior, "%b") |> stringr::str_to_title()
ano_corrente <- year(data_mes_anterior)


# Carregar base de dados direto do PEP
# pep_folder <- "Y:/PEP/PEP_reload/PEP_qvd_InOutrasFontes/Infograficos/
# pep_folder <- "C:/Users/wesley.jesus/Documents/Monitoramento_politicas_SGP/monitoramento_cotas"

# df <- readr::read_delim(file.path(pep_folder, "/etnia_raca.csv"),
#                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



# conectando databricks
use_virtualenv(Sys.getenv("venv_path"), required = TRUE)
sc <- spark_connect(
  master     = Sys.getenv("master"),
  method     = Sys.getenv("method"),
  cluster_id = Sys.getenv("cluster_id"),
  token      = Sys.getenv("token_databricks"),
  envname    = Sys.getenv("venv_path")
)


df <- sparklyr::spark_read_csv(sc,
                               name = 'tmp2',
                               delimiter = ";",
                               "/Volumes/mgi-bronze/raw_data_volumes/mgi/cginf/etnia_raca.csv")




# Etnia = Negras(Preta +Pardo) e demais

df <- df  |> 
  #filter( stringr::str_detect(df$`Nível Função2`, '^(FCE)|(CCE)'))  |>
  mutate(
    Etnia = case_when( 
      `Cor Origem Etnica` %in% c(4, 6)  ~ "Negras",
      .default = "Demais Raça/Cor"
    )
  ) |> janitor::clean_names()


# Carregar base de dados direto do repositorio

#df <- readr::read_csv("data/etnia_raca.csv")

# Conferencia com o PEP

df_conf <- df |>
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
  collect() %>%
  mutate(ano_mes := as.yearmon(`Mês-Ano Cargos`, "%B %Y")) 
  # filter(!`Decreto Nivel` %in% c("Nível 18"))
                             






### Indicador 1: % de negros (por órgão) ----

tabela <- df |>  
  filter(agrupamento_geral == 'CCE & FCE', 
         ano_cargos == ano_corrente,
         mes_cargos == mes_anterior_abreviado) |> 
  group_by(
    orgao_superior_cargos_e_funcoes,
    orgao_vinculado_cargos_e_funcoes,
    etnia ,
    # sexo,
    decreto_nivel
    ) |>
  dplyr::summarise(
    total = sum(quantidade_de_vinculos_cargos_e_funcoes)
    ) |> 
  ungroup() %>%
  collect() %>% 
  filter(!decreto_nivel %in% c("Nível 18"))   %>%
  group_by(orgao_vinculado_cargos_e_funcoes, decreto_nivel) |> 
  mutate(
    p_nivel = round(100*total/sum(total, na.rm = TRUE),1)
  ) |>  
  ungroup() %>%
  filter(etnia == "Negras") %>%
  select(-total,-etnia) %>%
  # janitor::adorn_totals()
  tidyr::pivot_wider(
    names_from = decreto_nivel,
    values_from = p_nivel,
    values_fill = 0,
    names_sort = T) %>%
  rename(
    `Órgão Superior` = orgao_superior_cargos_e_funcoes,
    `Órgão` = orgao_vinculado_cargos_e_funcoes
    )  %>% #View
  arrange(`Nível 13 a 17`,`Nível 1 a 12`)
  # rowwise()  %>% 
  # mutate(
  #   Total = sum(c_across(Fem:Mas), na.rm = TRUE
  #   )
  # ) #%>% #View
  # filter(`Cargo-Função` %in% c("Nível 1 a 12", "Nível 13 a 17"))



# Salvar base tratada
saveRDS(tabela, "data/Tab.rds")


### Indicador 1: % de negros (por órgão superior) ----

tabela_sup <- df |>  
  filter(agrupamento_geral == 'CCE & FCE', 
         ano_cargos == ano_corrente,
         mes_cargos == mes_anterior_abreviado) |> 
  group_by(
    orgao_superior_cargos_e_funcoes,
    etnia ,
    # sexo,
    decreto_nivel
  ) |>
  dplyr::summarise(
    total = sum(quantidade_de_vinculos_cargos_e_funcoes)
  ) |> 
  ungroup() %>%
  collect() %>% 
  filter(!decreto_nivel %in% c("Nível 18"))   %>%
  group_by(orgao_superior_cargos_e_funcoes, decreto_nivel) |> 
  mutate(
    p_nivel = round(100*total/sum(total, na.rm = TRUE),1)
  ) |>  
  ungroup() %>%
  filter(etnia == "Negras") %>%
  select(-total,-etnia) %>%
  # janitor::adorn_totals()
  tidyr::pivot_wider(
    names_from = decreto_nivel,
    values_from = p_nivel,
    values_fill = 0,
    names_sort = T) %>%
  rename(
    `Órgão Superior` = orgao_superior_cargos_e_funcoes
  ) %>% #View
  arrange(`Nível 13 a 17`,`Nível 1 a 12`)#%>% #View


# Salvar base tratada
saveRDS(tabela_sup, "data/Tab_sup.rds")


### Indicadores 1: % de negros (mensal) ----


## agregados por mês e Etnia
comissionados_negros_mes <- df |>  
  filter(agrupamento_geral == 'CCE & FCE',
         orgao_vinculado_cargos_e_funcoes != "Agencia Brasileira De Inteligencia") |>
  group_by(
    mes_ano_cargos,
    etnia,
    # sexo,
    decreto_nivel
  ) |>
  dplyr::summarise(
    total = sum(quantidade_de_vinculos_cargos_e_funcoes)
  ) |> 
  ungroup() %>%
  collect() |>
  filter(!decreto_nivel %in% c("Nível 18")) |>  
  mutate(anomes = as.yearmon(mes_ano_cargos,"%B %Y")) 

## percentuais
setDT(comissionados_negros_mes)
comissionados_negros_mes[,`:=`(p_negras = 100*total/sum(total)),
                 .(mes_ano_cargos,decreto_nivel)]
comissionados_negros_mes <- comissionados_negros_mes[etnia %in% "Negras"]


# Salvar base tratada
saveRDS(comissionados_negros_mes, "data/Tab_inds_1_e_2.rds")

########################################################################.
### Indicador 3: necessidade de cargos vagos ----
########################################################################.


## 3.1 carregando e ajustando total de cargos distribuídos ----


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


## 3.2 total de cargos ocupados no mesmo mês de cargos distribuídos ----

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
  collect() %>%
  setDT %>%
  .[,Total_ocupados := sum(total_negras),
    .(orgao_superior_cargos_e_funcoes,
      orgao_vinculado_cargos_e_funcoes,
      decreto_nivel)] %>% 
  filter(etnia == "Negras") %>%
  select(-etnia)

### 3.2  cruzamento com cargos vagos ----
tabela_vagos[,`:=`(orgao_sup_clean = limpa_string(orgao_superior_cargos_e_funcoes),
                   orgao_vinc_clean = limpa_string(orgao_vinculado_cargos_e_funcoes)
                )]



## de-para fuzzy, órgão vinculado
de_para <- stringdist_left_join(tabela_vagos[,.(orgao_vinc_clean)] %>% unique,
                                cargos_disp[,.(orgao)] %>% unique,
                                distance_col = 'dist',
                                method = "cosine",
                                q = 6,
                                max_dist = 0.8,
                                by = c("orgao_vinc_clean" = "orgao")
                                ) %>%
  setorder(orgao_vinc_clean,dist) %>%
  group_by(orgao_vinc_clean) %>%
  slice_head(n=1) %>%
  ungroup %>%
  setDT %>%
  filter(!grepl("(^vice)|(inteligencia$)",orgao_vinc_clean),
         dist < 0.75)
                                




tabela_vagos <-
  tabela_vagos %>%
  left_join(de_para[,.(orgao_vinc_clean,orgao)],
            by = "orgao_vinc_clean") %>% 
  left_join(cargos_niveis_disp[,.(orgao,
                                  cargo_funcao,
                                  total_dist)],
            by = c("orgao",
                   "decreto_nivel" = "cargo_funcao")) %>% 
  # .[,total_dist := ifelse(is.na(total_dist.x),total_dist.y,total_dist.x)] %>% 
  select(-orgao_sup_clean,-orgao_vinc_clean)#,
         # -total_dist.x,-total_dist.y) 


# ajuste 1: se ocupados >  distribuídos, distribuídos = ocupados
tabela_vagos[,total_dist := ifelse(total_dist < Total_ocupados,Total_ocupados,total_dist)]

# ajuste 2: 'algoritmo' para necessidade
tabela_vagos[,linha:=.I]
tabela_vagos[,necessidade_vagas := nec_meta(Total_ocupados,total_negras)]

# ajuste 3: cargos disponíveis sempre positivo
tabela_vagos[,`:=`( cargos_disponiveis = total_dist - Total_ocupados)]
tabela_vagos[,`:=`( 
  # indice_necessidade =
    # ifelse(necessidade_vagas == 0,
    #        0,
    #        ifelse(cargos_disponiveis == 0,
    #               99,
    #               necessidade_vagas/cargos_disponiveis)
    #        ),
  indice_suficiencia = cargos_disponiveis/necessidade_vagas
  )]

Tab_ind3 <- 
  select(tabela_vagos,
         orgao_superior_cargos_e_funcoes,
         orgao_vinculado_cargos_e_funcoes,
         decreto_nivel,
         Total_ocupados,
         total_negras,
         total_dist,
         necessidade_vagas,
         cargos_disponiveis,
         indice_suficiencia
  ) %>%
  filter(!is.na(indice_suficiencia),
         necessidade_vagas > 0,
         orgao_vinculado_cargos_e_funcoes != "Agencia Brasileira De Inteligencia") %>%
  setorder(indice_suficiencia)

# Salvar base tratada
saveRDS(Tab_ind3, "data/Tab_ind3.rds")


########################################################################.
### Indicador 4: necessidade de cargos vagos ----
########################################################################.

####
## 4.1 Extração de dados ----
###




# ler dados de volume parquet no databricks   
df_parquet <- sparklyr::spark_read_parquet(sc,
                                           name = "tmp_parquet",
                                           "/Volumes/mgi-bronze/raw_data_volumes/mgi/cginf/servidores_dwsiape_etnia_efetivos")  




# agrupando e extraindo
df_parquet %>%
  group_by(mes,nome_mes,
           orgao_vinc,nome_orgao_vinc,
           # sexo,nome_sexo,
           cor_origem_etnica,nome_cor_origem_etnica,
           # funcao,nome_funcao
           ) %>%
  filter(nome_orgao_vinc != "ABIN") %>%
  summarise(qtde = sum(qtde_vinculos,na.rm = T)) %>%
  arrange(mes,desc(qtde)) %>%
  collect() %>%
  setDT -> total_etnia_mes




####
## 4.2 Juntando totais servidores com totais comissionados ----
####


# agrupando comissionados
comissionados_etnia <- df |>  
  filter(funcao == 'FEX',
         orgao_vinculado_cargos_e_funcoes != "Agencia Brasileira De Inteligencia") |>
  group_by(
    cod_orgao_cargos_e_funcoes,
    orgao_superior_cargos_e_funcoes,
    orgao_vinculado_cargos_e_funcoes,
    mes_ano_cargos,
    nome_cor_origem_etnica,
    # sexo,
    decreto_nivel
  ) |>
  dplyr::summarise(
    comissao = sum(quantidade_de_vinculos_cargos_e_funcoes)
  ) |> 
  ungroup() |>
  collect() %>%
  filter(!decreto_nivel %in% c("Nível 18")) |>  
  mutate(anomes = as.yearmon(mes_ano_cargos,"%B %Y"))  %>%
  tidyr::pivot_wider(
    names_from =decreto_nivel,
    values_from = comissao,
    values_fill = 0)
  

####
## 4.3 Indicador 4 por etnia e mês ----
####

## jutando as duas coisas
comissionados_etnia <- 
  inner_join(
    comissionados_etnia,
    total_etnia_mes %>%
      select(nome_mes,orgao_vinc,nome_orgao_vinc,nome_cor_origem_etnica,qtde),
    by = c("mes_ano_cargos" = "nome_mes",
           "cod_orgao_cargos_e_funcoes" = "orgao_vinc",
           "nome_cor_origem_etnica")
    ) %>%
  setDT


## indicadores por mês
comissionados_etnia_mes <-
  comissionados_etnia[,lapply(.SD,sum,na.rm = T),
                      .(nome_cor_origem_etnica,
                        anomes),
                      .SDcols = c("Nível 1 a 12","Nível 13 a 17","qtde")
                      ] %>%
  melt(id.vars = c("anomes","nome_cor_origem_etnica")) %>% 
  .[,p_etnia := round(100*value/sum(value),2),
    .(anomes,variable)] %>%   
  select(-value) %>%
  tidyr::pivot_wider(
    names_from = variable,
    values_from = p_etnia,
    values_fill = 0) %>%
  filter(nome_cor_origem_etnica != "NAO INFORMADO") %>%
  rbind(.,
        
        # comissionados_negros_mes <-
        comissionados_etnia[,lapply(.SD,sum,na.rm = T),
                            .(nome_cor_origem_etnica = 
                                ifelse(nome_cor_origem_etnica %in% c("PARDA","PRETA"),
                                       "Negras",
                                       "Demais Raça/Cor"),
                              anomes),
                            .SDcols = c("Nível 1 a 12","Nível 13 a 17","qtde")
                            ] %>%
          melt(id.vars = c("anomes","nome_cor_origem_etnica")) %>% 
          .[,p_etnia := round(100*value/sum(value),2),
            .(anomes,variable)]%>%   
          select(-value) %>%   
          tidyr::pivot_wider(
            names_from =variable,
            values_from = p_etnia,
            values_fill = 0) %>% 
          filter(nome_cor_origem_etnica != "Demais Raça/Cor")
        ) %>%
  setDT %>%
  .[,`:=`(ind4_1_a_12 = `Nível 1 a 12`/qtde,
          ind4_13_a_17 = `Nível 13 a 17`/qtde)]



# Salvar base tratada
saveRDS(comissionados_etnia_mes, "data/Tab_inds_4_mes.rds")

####
## 4.4 Indicador 4 por etnia e órgão ----
####

anomes_anterior <- as.yearmon(Sys.Date() %m-% months(1))


setnames(comissionados_etnia,
         gsub("N.v","Niv",names(comissionados_etnia))
         )

# comissionados_negros_mes <-
comissionados_etnia %>%
  filter(anomes == anomes_anterior) %>% #NROW
  .[,lapply(.SD,sum,na.rm = T),
    .(cod_orgao_cargos_e_funcoes,
      orgao_superior_cargos_e_funcoes,
      orgao_vinculado_cargos_e_funcoes,
      nome_cor_origem_etnica = 
        ifelse(nome_cor_origem_etnica %in% c("PARDA","PRETA"),
               "Negras",
               ifelse(nome_cor_origem_etnica %in% "BRANCA",
                      "Brancas",
                      "Demais Raca/Cor")
        )
      ),
    .SDcols = c("Nivel 1 a 12","Nivel 13 a 17","qtde")
    ] %>% 
  melt(measure.vars = c("Nivel 1 a 12","Nivel 13 a 17","qtde")) %>%  
  .[,p_etnia := round(100*value/sum(value),2),
    .(cod_orgao_cargos_e_funcoes,
      orgao_superior_cargos_e_funcoes,
      orgao_vinculado_cargos_e_funcoes,
      variable)] %>%  
  select(-value) %>%   
  tidyr::pivot_wider(
    names_from =variable,
    values_from = p_etnia,
    values_fill = 0) %>%
  setDT %>%
  .[,`:=`(ind4_1_a_12 = round(`Nivel 1 a 12`/qtde,2),
          ind4_13_a_17 = round(`Nivel 13 a 17`/qtde,2))]-> comissionados_etnia_orgao


# Salvar base tratada
saveRDS(comissionados_etnia_orgao, "data/Tab_inds_4_orgaos.rds")



########################################################################.
### Indicador 5:  Índice de equidade remuneratória por raça ----
########################################################################.



####
## 5.1 carregando salários por nível FCE e CCE ----
####

## carregando 
salarios_niveis <- read_excel("data/tabela_remuneracao_fce_cce.xlsx") %>%
  setDT %>%
  janitor::clean_names()

## separando níveis
salarios_niveis[,`:=`(
  agrupamento = str_extract_all(cargo_funcao,"FCE|CCE") %>% unlist,
  nivel = str_extract_all(cargo_funcao,"[0-9]{2}$") %>% unlist
)] %>%
  select(agrupamento,nivel,valor) %>%
  unique -> salarios_niveis


####
## 5.2 contar por órgão, mês, etnia, agrupamento FCE e CCE e nível ----
####

## agregados
renda_etnia_nivel <- df |>  
  filter(agrupamento_geral == 'CCE & FCE',
         orgao_vinculado_cargos_e_funcoes != "Agencia Brasileira De Inteligencia") %>%
  group_by(
    orgao_superior_cargos_e_funcoes,
    orgao_vinculado_cargos_e_funcoes,
    mes_ano_cargos,
    funcao,
    nivel_funcao,
    nome_cor_origem_etnica,
    sexo
  ) %>% 
  dplyr::summarise(
    total = sum(quantidade_de_vinculos_cargos_e_funcoes)
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(nivel_cce_fce = str_extract_all(nivel_funcao,"[0-9]{2}$") %>% unlist)  |>
  group_by(
    orgao_superior_cargos_e_funcoes,
    orgao_vinculado_cargos_e_funcoes,
    mes_ano_cargos,
    funcao,
    nivel_cce_fce,
    nome_cor_origem_etnica,
    sexo
  ) %>% 
  dplyr::summarise(
    total = sum(total)
  ) %>% 
  ungroup() %>%
  filter(!nivel_cce_fce %in% c("18"),
         nome_cor_origem_etnica %in% c("BRANCA","PARDA","PRETA")) %>% 
  mutate(anomes = as.yearmon(mes_ano_cargos,"%B %Y"),
         funcao = gsub("(CX|EX)","CE",funcao),
         nome_cor_origem_etnica = stringr::str_to_title(nome_cor_origem_etnica),
         cor_etnia_ag = ifelse(nome_cor_origem_etnica %in% c("Parda","Preta"),
                               "Negra",
                               ifelse(nome_cor_origem_etnica %in% "Branca",
                                      "Branca",
                                      "Demais Raca/Cor") %>%
                                 stringr::str_to_title()
         )) %>%
  setDT

## juntando com salários
renda_etnia_nivel <- 
  left_join(renda_etnia_nivel,
            salarios_niveis,
            by = c("funcao" = "agrupamento",
                   "nivel_cce_fce" = "nivel")
            )

## massa salarial no nível
renda_etnia_nivel[,`:=`(massa_salarial = total*valor)]


####
## 5.3 Indicador 5 por etnia e mês ----
####

# renda_etnia_nivel[,.(total = sum(total)),
#                   .(anomes,funcao,
#                     nivel_cce_fce = ifelse(as.numeric(nivel_cce_fce) <= 12,'0','1'))] %>% 
#   ggplot(aes(x = anomes,y = total,col = nivel_cce_fce)) + 
#   geom_line() + 
#   facet_wrap(funcao ~ .) + 
#   geom_point()

## indicadores por mês
renda_etnia_mes <-
  renda_etnia_nivel[,lapply(.SD,sum,na.rm = T),
                    .(nome_cor_origem_etnica,
                      anomes),
                      .SDcols = c("total","massa_salarial")
  ] %>% 
  .[,salario_medio := massa_salarial/total] %>%
  rbind(.,
        renda_etnia_nivel[cor_etnia_ag == "Negra",
                          lapply(.SD,sum,na.rm = T),
                          .(nome_cor_origem_etnica = cor_etnia_ag,
                            anomes),
                          .SDcols = c("total","massa_salarial")]%>% 
          .[,salario_medio := round(massa_salarial/total,1)] ,
        fill = T) %>% 
  left_join(.,
            .[nome_cor_origem_etnica == "Branca",
              .(anomes,medio_brancos = salario_medio)]) %>% 
  .[,ind5 := round(salario_medio / medio_brancos,2)] %>%
  select(-massa_salarial,-medio_brancos,-total)

# Salvar base tratada
saveRDS(renda_etnia_mes, "data/Tab_inds_5_mes.rds")



####
## 5.4 Indicador 5 por etnia e órgão ----
####

anomes_anterior <- as.yearmon(Sys.Date() %m-% months(2))


setnames(comissionados_etnia,
         gsub("N.v","Niv",names(comissionados_etnia))
)

renda_etnia_orgao <-
  renda_etnia_nivel[anomes == anomes_anterior,
                    lapply(.SD,sum,na.rm = T),
                    .(orgao_superior_cargos_e_funcoes,
                      orgao_vinculado_cargos_e_funcoes,
                      nome_cor_origem_etnica),
                    .SDcols = c("total","massa_salarial")
  ] %>% #View
  .[,salario_medio := massa_salarial/total] %>%
  rbind(.,
        renda_etnia_nivel[cor_etnia_ag == "Negra" & 
                            anomes == anomes_anterior,
                          lapply(.SD,sum,na.rm = T),
                          .(orgao_superior_cargos_e_funcoes,
                            orgao_vinculado_cargos_e_funcoes,
                            nome_cor_origem_etnica = cor_etnia_ag),
                          .SDcols = c("total","massa_salarial")]%>% 
          .[,salario_medio := massa_salarial/total] ,
        fill = T) %>% 
  left_join(.,
            .[nome_cor_origem_etnica == "Branca",
              .(orgao_superior_cargos_e_funcoes,
                orgao_vinculado_cargos_e_funcoes,
                medio_brancos = salario_medio)],
            by = c("orgao_superior_cargos_e_funcoes",
                   "orgao_vinculado_cargos_e_funcoes")) %>% 
  .[,ind5 := round(salario_medio / medio_brancos,1)] %>%
  select(-massa_salarial,-salario_medio,-medio_brancos,-total) %>%
  filter(nome_cor_origem_etnica != "Branca") %>%  #View
  tidyr::pivot_wider(
    names_from =nome_cor_origem_etnica,
    values_from = ind5,
    values_fill = 0)

# Salvar base tratada
saveRDS(renda_etnia_orgao, "data/Tab_inds_5_orgaos.rds")



####
## 5.5 Distribuição de níveis por etnia ----
####

renda_etnia_nivel[anomes == anomes_anterior & 
                    nivel_cce_fce != '18',
                  .(total = sum(total)),
                  .(nome_cor_origem_etnica,
                    funcao,
                    nivel_cce_fce = as.numeric(nivel_cce_fce)
                  )]  %>%
  rbind(.,
        renda_etnia_nivel[anomes == anomes_anterior & 
                            nivel_cce_fce != '18' & 
                            cor_etnia_ag == "Negra",
                          .(nome_cor_origem_etnica = "Negra",
                            total = sum(total)),
                          .(funcao,
                            nivel_cce_fce = as.numeric(nivel_cce_fce)
                            )])-> nivel_etnia_funcao #%>%
# .[,p_cce := 100*total/sum(total),
#   .(nome_cor_origem_etnica,nivel_cce_fce)] %>%  #View
# filter(funcao == 'CCE') %>%
# ggplot(aes(x = (nivel_cce_fce),
#            y = p_cce,
#            col = nome_cor_origem_etnica,
#            fill = nome_cor_origem_etnica)) +
# geom_col(position = 'dodge') +
# # geom_line(size = 2) +
# scale_x_continuous(breaks = 1:17,labels = 1:17) +
# geom_text(aes(label = round(p_cce,1)),
#           position = position_dodge(width = 0.9),
#           hjust = 0.7,
#           vjust = -0.9,
#           show.legend = F
#            # position = position_stack()
#            ) #+
# facet_wrap(nome_cor_origem_etnica ~. ,ncol = 1)


saveRDS(nivel_etnia_funcao, "data/Tab_inds_5_niveis.rds")



# desconectando
sparklyr::spark_disconnect(sc)
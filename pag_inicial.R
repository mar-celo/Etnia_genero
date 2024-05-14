## code to prepare `DATASET` dataset goes here
install.packages(c("renv", "remotes", "dplyr", "tidyr", "lubridate", "janitor", "readr", "DT", "plotly", "rmarkdown",
                   "echarts4r", "htmltools", "stringr", "crosstalk", "readxl", "RColorBrewer", 
                   "reactablefmtr", "sysfonts", "showtext", "htmlwidgets" ))

pacotes <- renv::dependencies() |>
  dplyr::filter(!Package %in% c("renv", "dplyr")) |>
  dplyr::pull(Package) |>
  unique()
remotes::install_github("timelyportfolio/dataui")
install.packages(pacotes)



# Carregar pacotes
library(rmarkdown)
library(DT)
library(lubridate)
library(plotly)
library(crosstalk)
library(readxl)
library(RColorBrewer)
library(reactablefmtr)
library(sysfonts)
library(showtext)
library(htmltools)
library(htmlwidgets)

#Por órgão 

dados <- readRDS("data/Tab.rds") |> 
  filter(`Órgão` != "Agencia Brasileira De Inteligencia")   |>  
  dplyr::rename(`Raça/Cor` = etnia, 
                `%` = `% Cargo-Função/Etnia`)

sd_total <- SharedData$new(dados, group = "funcao")

# Retirar ABIN 

data_funcao <- dados |> select(`Órgão Superior`,
                               Órgão, `Raça/Cor`, 
                               `Cargo-Função`, Fem, Mas, Total) |> 
  tidyr::pivot_wider(names_from = `Cargo-Função`, values_from = Total) |> 
  filter(Órgão != "Agencia Brasileira De Inteligencia")


sd_pizza <- SharedData$new(data_funcao, group = "funcao")

mes <- format(Sys.Date() %m-% months(1), "%B de %Y") |> stringr::str_to_title()

filtro_Orgao_pizza <- filter_select("Orgao_PIZZA", 
                                    paste0("Selecione um Órgão: ", "(",mes, ")"),
                                    sd_pizza, 
                                    allLevels = TRUE,
                                    ~Órgão,
                                    multiple = FALSE
)

pier_Nivel1a12 <- plot_ly() |>  
  add_pie(
    data = sd_pizza,
    labels = ~`Raça/Cor`,
    values = ~`Nível 1 a 12`,
    name = "Nível 1 a 12"
    #domain = list(x = c(0, 0.4), y = c(0, 1))
  )   

pier_Nivel13a17 <- plot_ly() |>  
  add_pie(
    data = sd_pizza,
    labels = ~`Raça/Cor`,
    values = ~`Nível 13 a 17`,
    name = "Nível 13 a 17"
    #domain = list(x = c(0.6, 1), y = c(0, 1))
  )   


figpier_Nivel1a12 <- pier_Nivel1a12 |>  
  layout(title = "Nível 1 a 12",
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'),
         showlegend=TRUE,showlegend2=TRUE, 
         margin = 0.01) 

figpier_Nivel13a17 <- pier_Nivel13a17 |>  
  layout(title = "Nível 13 a 17",
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'),
         showlegend=TRUE,showlegend2=TRUE, 
         margin = 0.01) 


# table

# Definir as configurações da tabela, incluindo o idioma em Português Brasil


# Criar a tabela usando datatable com as configurações personalizadas
configuracao <- list(
  language = list(
    infoFiltered = "(filtrado num total de _MAX_ registros)",
    info = "Exibindo _START_ até _END_ de _TOTAL_ registros",
    lengthMenu = "Mostrar _MENU_ registros por página",
    search = "Pesquisar:",
    paginate = list(
      first = "Primeiro",
      last = "Último",
      `next` = "Próximo",
      previous = "Anterior"),
    aria = list(
      sortAscending = ": ativar para classificar coluna em ordem crescente",
      sortDescending = ": ativar para classificar coluna em ordem decrescente"
    )
  ),
  dom = 'Bfrtip',
  buttons = c('csv', 'excel', 'pdf', 'print')
  
)


dt2 <- sd_total |> 
  DT::datatable(#filter = "top",
    #style = "bootstrap",
    class = "compact",
    width = "100%",
    extensions = 'Buttons',
    options = configuracao
  )

# gerar para a tela inicial

# Gere o html_object e salve o Viewer como index.html na pasta observatório
html_object <- bscols(
  # ranking_1_a_12,
  # ranking_13_a_17,
  filtro_Orgao_pizza,
  figpier_Nivel1a12,
  figpier_Nivel13a17,
  widths = c(
    #6,6,
    12, 6,6),
  device = "sm")

# bscols(filtro_Orgao_pizza, widths = 6)

# salvar html_object como index.html


library(htmltools)

save_html(html_object, file = "observatorio/index.html")

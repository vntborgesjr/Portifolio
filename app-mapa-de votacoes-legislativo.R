# --------------------------------------------------- 
# Mapa de votação do legislativo entre 2019 e 2020 - versão App
# 20 nov 2020 
# VNTBJR 
# --------------------------------------------------- 
#
# Carregar pacotes  -------------------------------------------
library(tidyverse)
library(rgdal)
library(tmap)
library(shiny)

# Carregar dados  -------------------------------------------
cham_19 <- read_csv("datasets/cham_19.csv")[, -1]
cham_20 <- read_csv("datasets/cham_20.csv")[, -1]
regiao_shp <- readOGR(dsn = "regioes_2010", layer = "regioes_2010")
estados_shp <- readOGR(dsn = "estados_2010", layer = "estados_2010")

# Agrupar dados das votações  -------------------------------------------
legislativo <- rbind(cham_19, cham_20)

# sumarizar dados ---------------------------------------------------------
# Resultado por matéria
resultado_materia <- legislativo %>% 
  group_by(rollcall_id, ano) %>% 
  mutate(vote = ifelse(legislator_vote == "Sim", 1, 
                       ifelse(legislator_vote == "Nao", 1, 
                              ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  group_by(rollcall_id, Votos = legislator_vote, ano) %>% 
  filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  summarise(vote = sum(vote)) %>% 
  ungroup() %>% 
  select(materia = rollcall_id, Votos, total = vote)

# estado
estado_votos <- legislativo %>% 
  group_by(rollcall_id, legislator_state, ano) %>% 
  mutate(total = ifelse(legislator_vote == "Sim", 1, 
                        ifelse(legislator_vote == "Nao", -1, 
                               ifelse(legislator_vote == "Art. 17", 0, 0))),
         sim = ifelse(legislator_vote == "Sim", 1, 
                      ifelse(legislator_vote == "Nao", 0, 
                             ifelse(legislator_vote == "Art. 17", 0, 0))),
         nao = ifelse(legislator_vote == "Sim", 0, 
                      ifelse(legislator_vote == "Nao", -1, 
                             ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  group_by(materia = rollcall_id, sigla = legislator_state, ano) %>% 
  filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  summarise(saldo = sum(total), sim = sum(sim), nao = sum(nao))

# partido
partido_votos <- legislativo %>% 
  group_by(rollcall_id, legislator_state, legislator_party, ano) %>% 
  mutate(saldo = ifelse(legislator_vote == "Sim", 1, 
                        ifelse(legislator_vote == "Nao", -1, 
                               ifelse(legislator_vote == "Art. 17", 0, 0))),
         sim = ifelse(legislator_vote == "Sim", 1, 
                      ifelse(legislator_vote == "Nao", 0, 
                             ifelse(legislator_vote == "Art. 17", 0, 0))),
         nao = ifelse(legislator_vote == "Sim", 0, 
                      ifelse(legislator_vote == "Nao", -1, 
                             ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  group_by(rollcall_id, legislator_state, legislator_party, legislator_vote, ano) %>% 
  filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  summarise(saldo = sum(saldo), sim = sum(sim), nao = sum(nao)) %>%
  ungroup() %>% 
  select(ano, materia = rollcall_id, sigla = legislator_state, 
         partido = legislator_party, nao, sim, saldo)

# Gerar objetos espaciais para as diferentes camadas  -------------------------------------------
estados_shp@data <- data.frame(estados_shp@data[, 3])
names(estados_shp@data) <- "sigla"

camadas_estados <- merge(x = estados_shp, y = estado_votos,
                          by = "sigla", 
                          duplicateGeoms = TRUE) # to merge a spatial object to a data frame

camadas_partidos <- merge(x = estados_shp, y = partido_votos,
                        by = "sigla", 
                        duplicateGeoms = TRUE) # to merge a spatial object to a data frame

camadas <- merge(x = camadas_partidos, y = resultado_materia,
                 by = "materia", 
                 duplicateGeoms = TRUE) # to merge a spatial object to a data frame
camadas@data$partido[camadas@data$partido == "CIDADANIA"] <- "Cidadania"
camadas@data$partido[camadas@data$partido == "NOVO"] <- "Novo"

# Funções para o servidor   -------------------------------------------
# gerador de dados para a situação das matérias
get_resu <- function(.ano, .materia) {
  camadas@data %>% 
    filter(ano == .ano) %>% 
    filter(materia ==  .materia) %>% 
    slice(1:2)
}

# gerador de dados para o mapa de votação por estado
get_estado <- function(.ano, .materia) {
  camadas_estados[camadas_estados@data$ano == .ano &
                    camadas_estados@data$materia == .materia, ]
}

# gerador de dados para o mapa de votações por estado por partido
get_partido <- function(.ano, .materia, .partido) {
  camadas[camadas@data$ano == .ano &
            camadas@data$materia == .materia &
            camadas@data$partido == .partido, ]
}

# Construir a interface do usuário  -------------------------------------------
iu <- fluidPage(
  titlePanel("Mapa de votação da Câmara dos Deputados Federais"),
  # Layout da barra lateral
  sidebarLayout(
    # posicionar seletores como painel lateral
    sidebarPanel(
      # seletor para o ano
      sliderInput(inputId = 'ano', label = 'Escolha um ano: ', 
                  min = 2019, max = 2020, value = 2019),
      # seletor de temas
      
      # seletor de matéria
      selectInput('materia', 'Selecione a matéria: ', 
                  choices = unique(camadas$materia)),
      # seletor de partido
      selectInput("partido", "Selecione um partido: ",
                  choices = unique(camadas$partido))
    ),
      # painel principal para o mapa
      mainPanel(
        tabsetPanel(
          # Aba da descrição e resultado da votação
          tabPanel("Resultado da votação", plotOutput("resultado")),
          # Aba do mapa com resultado da votação por estado
          tabPanel("Mapa: resultado por estado", tmap::tmapOutput("mapa_estado")),
          # Aba da tabela dos resultados da votação por estado
          tabPanel("Tabela: resutlado por estado", DT::DTOutput("tabela_estado")),
          # Aba do mapa com resultado da votação por estado por partido
          tabPanel("Mapa: resultado por estado por partido", tmap::tmapOutput("mapa_partido")),
          # Aba da tabela dos resultados da votação por estado por partido
          tabPanel("Tabela: resutlado por estado por partido", DT::DTOutput("tabela_partido"))
        )
        )))

servidor <- function(input, output, session) {
  # Expressão reativa para obter os dados para o resutlado da
  # situação da matéria
  resu_data <- reactive({
    get_resu(input$ano, input$materia)
  })
  # Situação da votação de uma matéria
  output$resultado <- renderPlot({
    d <- resu_data() 
      ggplot(d, aes(x = "", y = total, fill = Votos)) +
      geom_bar(colour = "black", stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      ggtitle(ifelse(d$total[d$Votos == "Sim"][1] > d$total[d$Votos == "Nao"][1],
                     paste("Situação da ", 
                           input$materia, "\nAprovada"), 
                     paste("Situação da ", 
                           input$materia, "\nReprovada"))) +
      theme_minimal() + 
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 12, 
                                      face = "bold", 
                                      hjust = 0.5))
    })
  
  # Expressão para obter os dados para o mapa de votação por 
  # estado
  resu_estado <- reactive({
    get_estado(input$ano, input$materia)
  })
  # Mapa da votação de uma matéria por estado
  output$mapa_estado <- tmap::renderTmap({
    e <- resu_estado()
    tmap_mode("view")
    mapa_estado_final <- tm_basemap(server = "CartoDB.PositronNoLabels") +
      # saldo da votação por estado
      tm_shape(shp = e, name = "Resultado por estado") +
      tm_fill(col = "saldo", midpoint = 0,
              title = paste0("Saldo (", e$materia[1], ")")) +
      tm_borders() +
      tm_text(text = "sigla", size = 1) +
      tm_symbols(alpha = 0, border.alpha = 0,
                 popup.vars = c(`Votos contra: ` = 'nao',
                                `Votos a favor: ` = 'sim',
                                `Saldo da votação: ` = "saldo")) +
      tm_borders(col = "black", lwd = 2) +
      tm_view(leaflet.options = c(dragging = FALSE), 
              set.zoom.limits = c(3, 10), 
              view.legend.position = c("right", "bottom")) +
      # Região
      tm_shape(regiao_shp) + 
      tm_borders(col = "black", lwd = 2)  
    
    mapa_estado_final
  })
  # tabela de dados que originou o mapa
  output$tabela_estado <- DT::renderDT({
    e <- resu_estado()
    data.frame(unique(e@data %>% 
                        select(Estado = sigla, Não = nao,
                               Sim = sim, Saldo = saldo)), 
               row.names = NULL)
    
  })
  # Expressão para obter os dados para o mapa de votação por 
  # estado por partido
  resu_partido <- reactive({
    get_partido(input$ano, input$materia, input$partido)
  })
  # Mapa da votação de uma matéria por partido
  output$mapa_partido <- tmap::renderTmap({
    m <- resu_partido()
    tmap_mode("view")
    mapa_partidos_final <- tm_basemap(server = "CartoDB.PositronNoLabels") +
      # saldo por  partido por estado
      tm_shape(shp = m, name = paste0("Saldo da votação: ", unique(m$partido))) +
      tm_fill(col = "saldo", 
              title = paste0(m$partido, " (", m$materia, ")")) +
      tm_borders() +
      tm_text(text = "sigla", size = 1) +
      tm_symbols(alpha = 0, border.alpha = 0,
                 popup.vars = c(`Saldo da votação: ` = "saldo",
                                `Votos a favor: ` = "sim",
                                `Votos contra: ` = "nao")) +
      tm_view(leaflet.options = c(dragging = FALSE), 
              set.zoom.limits = c(3, 10), view.legend.position = c("right", "bottom")) +
      # Região
      tm_shape(regiao_shp) +
      tm_borders(col = "black", lwd = 2)  
    
    mapa_partidos_final
  })
  # tabela de dados que originou o mapa
  output$tabela_partido <- DT::renderDT({
    m <- resu_partido()
    data.frame(unique(m@data %>% 
                        select(Estado = sigla, Partido = partido, 
                               Não = nao, Sim = sim, Saldo = saldo)), 
               row.names = NULL)
  })
}

shinyApp(ui = iu, server = servidor)


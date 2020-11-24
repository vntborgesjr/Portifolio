# --------------------------------------------------- 
# Mapa de votação do legislativo entre 2015 e 2020 - versão App
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
cham_18 <- read_csv("datasets/cham_18.csv")[, -1]
cham_19 <- read_csv("datasets/cham_19.csv")[, -1]
cham_20 <- read_csv("datasets/cham_20.csv")[, -1]
regiao_shp <- readOGR(dsn = "regioes_2010", layer = "regioes_2010")
estados_shp <- readOGR(dsn = "estados_2010", layer = "estados_2010")

# Agrupar dados das votações  -------------------------------------------
legislativo <- rbind(cham_18, cham_19, cham_20)

# sumarizar dados ---------------------------------------------------------
# Resultado por matéria
resultado_materia <- legislativo %>% 
  group_by(rollcall_id, ano) %>% 
  mutate(vote = ifelse(legislator_vote == "Sim", 1, 
                       ifelse(legislator_vote == "Nao", 1, 
                              ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  group_by(rollcall_id, Votos = legislator_vote, ano) %>% 
  filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  summarise(vote = sum(vote))

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
  group_by(rollcall_id, legislator_state, ano) %>% 
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
  summarise(saldo = sum(saldo), sim = sum(sim), nao = sum(nao))

# Gerar objetos espaciais para as diferentes camadas  -------------------------------------------
camada_estado <- merge(x = estados_shp, y = estado_votos,
                       by.x = "sigla", by.y = "legislator_state", 
                       duplicateGeoms = TRUE) # to merge a spatial object to a data frame

camada_partido <- merge(x = estados_shp, y = partido_votos,
                        by.x = "sigla", by.y = "legislator_state", 
                        duplicateGeoms = TRUE) # to merge a spatial object to a data frame

# gerando camadas saldo, sim e não de mapa por estado
camada_estado <- camada_estado[, c("rollcall_id", "sigla",
                                   "nao", "sim", "saldo")]
estado <- list()

for (i in 3:5) {
  estado[[i]] <- camada_estado[, 
                               c(1, 2, i)]
}

estado <- c(nao = estado[[3]], sim = estado[[4]], saldo = estado[[5]])
head(estado$nao)
# gerando camadas saldo, sim e não de mapa por partido
index <- length(levels(factor(camada_partido@data$legislator_party)))
partidos_nao <- list()
partidos_sim <- list()
partidos_saldo <- list()

for (i in 1:index){
  # votos nao por partido por estado
  partidos_nao[[i]] <- camada_partido[camada_partido$legislator_party == levels(factor(camada_partido$legislator_party))[i],
                                      c("rollcall_id", "sigla", "legislator_party", "nao")]
  names(partidos_nao)[[i]] <- levels(factor(camada_partido@data$legislator_party))[i]
  # votos sim por partido por estado
  partidos_sim[[i]] <- camada_partido[camada_partido$legislator_party == levels(factor(camada_partido$legislator_party))[i],
                                      c("rollcall_id", "sigla", "legislator_party", "sim")]
  names(partidos_sim)[[i]] <- levels(factor(camada_partido@data$legislator_party))[i]
  # saldo da votação por partido por estado
  partidos_saldo[[i]] <- camada_partido[camada_partido$legislator_party == levels(factor(camada_partido$legislator_party))[i],
                                        c("rollcall_id", "sigla", "legislator_party", "nao", "sim", "saldo")]
  names(partidos_saldo)[[i]] <- levels(factor(camada_partido@data$legislator_party))[i]
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
                  min = 2018, max = 2020, value = 2020),
      # seletor de temas
      
      # seletor de matéria
      selectInput('rollcall_id', 'Selecione a matéria: ', 
                  choices = camada_estado$rollcall_id),
      # seletor de partido
      selectInput("legislator_party", "Selecione um partido: ",
                  choices = camada_partido$legislator_party)
      # seletor de votação
      #selectInput('nao', 'Selecione o resultado: ', 
      #            choices = c('nao', 'sim', 'saldo'), selected = 'saldo'),
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
          tabPanel("Tabela: resutlado por estado", DT::DTOutput("tabela_partido"))
        )
        )))

servidor <- function(input, output, session) {
  # Função para selecionar os dados por estado - precisa selecionar 
  # os dados primeiro senão vai dar errado
  plot_resultado_materia <- function() {
    resultado_materia %>% 
      filter(ano == 2020) %>% 
      filter(rollcall_id ==  "MPV-897-2019-1") %>% 
      ggplot(aes(x = "", y = vote, fill = Votos)) +
      geom_bar(colour = "black", stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_minimal() + 
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 12, face = "bold"))
  }
  output$resultado <- renderPlot({
    plot_resultado_materia()
  })
  output$mapa_estado <- tmap::renderTmap({
    
  })
  output$tabela_estado <- DT::renderDT({
    
  })
  output$mapa_partido <- tmap::renderTmap({
    
  })
  output$tabela_partido <- DT::renderDT({
    
  })
}

shinyApp(ui = iu, server = servidor)

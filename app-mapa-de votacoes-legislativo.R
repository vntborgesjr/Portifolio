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
#estado_votos <- legislativo %>% 
 # group_by(rollcall_id, legislator_state, ano) %>% 
  #mutate(total = ifelse(legislator_vote == "Sim", 1, 
   #                     ifelse(legislator_vote == "Nao", -1, 
    #                           ifelse(legislator_vote == "Art. 17", 0, 0))),
     #    sim = ifelse(legislator_vote == "Sim", 1, 
      #                ifelse(legislator_vote == "Nao", 0, 
       #                      ifelse(legislator_vote == "Art. 17", 0, 0))),
        # nao = ifelse(legislator_vote == "Sim", 0, 
         #             ifelse(legislator_vote == "Nao", -1, 
          #                   ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  #group_by(rollcall_id, legislator_state, ano) %>% 
  #filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  #summarise(saldo = sum(total), sim = sum(sim), nao = sum(nao))

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

camadas_partidos <- merge(x = estados_shp, y = partido_votos,
                        by = "sigla", 
                        duplicateGeoms = TRUE) # to merge a spatial object to a data frame

camadas <- merge(x = camadas_partidos, y = resultado_materia,
                 by = "materia", 
                 duplicateGeoms = TRUE) # to merge a spatial object to a data frame

# Construir a interface do usuário  -------------------------------------------
iu <- fluidPage(
  titlePanel("Mapa de votação da Câmara dos Deputados Federais"),
  # Layout da barra lateral
  sidebarLayout(
    # posicionar seletores como painel lateral
    sidebarPanel(
      # seletor para o ano
      sliderInput(inputId = 'ano', label = 'Escolha um ano: ', 
                  min = 2019, max = 2020, value = 2020),
      # seletor de temas
      
      # seletor de matéria
      selectInput('materia', 'Selecione a matéria: ', 
                  choices = camadas$materia),
      # seletor de partido
      selectInput("partido", "Selecione um partido: ",
                  choices = camadas$partido)
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

plot_resultado_materia <- function() {
  
}
camadas@data %>% 
  filter(ano == 2020) %>% 
  filter(materia ==  "MPV-897-2019-1") %>% 
  slice(1:2) %>% 
  ggplot(aes(x = "", y = total, fill = Votos)) +
  geom_bar(colour = "black", stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle(ifelse(camadas@data$total[camadas@data$Votos == "Sim"][1] > camadas@data$total[camadas@data$Votos == "Nao"][1],
                 paste("Resutaldo da votação da ", 
                       camadas@data$materia[1], "\nAprovada"), 
                 paste("Resutaldo da votação da ", 
                       camadas@data$materia[1], "\nReprovada"))) +
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

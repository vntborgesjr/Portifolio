# --------------------------------------------------- 
# Mapa de votação do legislativo entre 2015 e 2020 - versão App
# 20 nov 2020 
# VNTBJR 
# --------------------------------------------------- 
#
# Carregar pacotes  -------------------------------------------
library(tidyverse)
library(tmap)
library(shiny)

# Carregar dados  -------------------------------------------
legislativo <- read_csv("datasets/cham_vote_15_20.csv")
regiao_shp <- readOGR(dsn = "regioes_2010", layer = "regioes_2010")
estados_shp <- readOGR(dsn = "estados_2010", layer = "estados_2010")



# sumarizar dados ---------------------------------------------------------
# estado
estado_votos <- legislativo %>% 
  group_by(rollcall_id, legislator_state) %>% 
  mutate(total = ifelse(legislator_vote == "Sim", 1, 
                        ifelse(legislator_vote == "Nao", -1, 
                               ifelse(legislator_vote == "Art. 17", 0, 0))),
         sim = ifelse(legislator_vote == "Sim", 1, 
                      ifelse(legislator_vote == "Nao", 0, 
                             ifelse(legislator_vote == "Art. 17", 0, 0))),
         nao = ifelse(legislator_vote == "Sim", 0, 
                      ifelse(legislator_vote == "Nao", -1, 
                             ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  group_by(rollcall_id, legislator_state) %>% 
  filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  summarise(saldo = sum(total), sim = sum(sim), nao = sum(nao))

# partido
partido_votos <- legislativo %>% 
  group_by(rollcall_id, legislator_state, legislator_party) %>% 
  mutate(saldo = ifelse(legislator_vote == "Sim", 1, 
                        ifelse(legislator_vote == "Nao", -1, 
                               ifelse(legislator_vote == "Art. 17", 0, 0))),
         sim = ifelse(legislator_vote == "Sim", 1, 
                      ifelse(legislator_vote == "Nao", 0, 
                             ifelse(legislator_vote == "Art. 17", 0, 0))),
         nao = ifelse(legislator_vote == "Sim", 0, 
                      ifelse(legislator_vote == "Nao", -1, 
                             ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  group_by(rollcall_id, legislator_state, legislator_party, legislator_vote) %>% 
  filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  summarise(saldo = sum(saldo), sim = sum(sim), nao = sum(nao))

# Gerar objetos espaciais para as diferentes camadas  -------------------------------------------
camada_estado <- merge(x = estados, y = estado_votos,
                       by.x = "sigla", by.y = "legislator_state", 
                       duplicateGeoms = TRUE) # to merge a spatial object to a data frame

camada_partido <- merge(x = estados, y = partido_votos,
                        by.x = "sigla", by.y = "legislator_state", 
                        duplicateGeoms = TRUE) # to merge a spatial object to a data frame

# gerando camadas saldo, sim e não de mapa por estado
camada_estado <- camada_estado[, c("rollcall_id", "sigla", "nao", "sim", "saldo")]
estado <- list()

for (i in 3:5) {
  estado[[i]] <- camada_estado[, 
                               c(1, 2, i)]
}

estado <- c(nao = estado[[3]], sim = estado[[4]], saldo = estado[[5]])

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
  titlePanel("Mapa de votação da Câmara dos Deputados Federais")),
  # seletor para o ano
  sliderInput(inputId = 'ano', label = 'Escolha um ano: ', 
              min = 2015, max = 2020, value = 2020),
  # seletor de temas
  
  # seletor de matéria
  selectInput('rollcall_id', 'Selecione a matéria', 
              choices = c(levels(factor(legislativo$rollcall_id))),
              selected = "primeira materia de 2020"),
  # seletor de mapa

  # seletor de votação
  selectInput('nao', 'Selecione o resultado: ', 
            choices = c('nao', 'sim', 'saldo'), selected = 'saldo'),
  # Add plot output to display top 10 most popular names
  plotOutput('plot_top_10_names')
)
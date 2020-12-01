# --------------------------------------------------- 
# Construindo mapa de votos - teste 1 
# 14 nov 2020 
# VNTBJR 
# --------------------------------------------------- 
#

# Mapa de votação da MPV-660-2014-1 -------------------------------------------
# Este mapa mostra o resultado da votação da MPV-660-2014-1 por estado.
# Votos a favor (sim) são representados por valores positivos.
# Votos contra (não) são representados por valores negativos.
# Na versão interativa
regiao <- readOGR(dsn = "regioes_2010", layer = "regioes_2010")
estados <- readOGR(dsn = "estados_2010", layer = "estados_2010")

summary(regiao)
summary(estados)

materia1 <- resultado_materia_estado1[resultado_materia_estado1$rollcall_id == resultado_materia_estado1$rollcall_id[1], ]
head(materia1)

materia2 <- resultado_materia_estado_partido[resultado_materia_estado_partido$rollcall_id == resultado_materia_estado_partido$rollcall_id[1], ]
head(materia2)

total <- merge(x = estados, y = materia1,
                by.x = "sigla", by.y = "legislator_state", 
                duplicateGeoms = TRUE) # to merge a spatial object to a data frame
head(total@data)

total2 <- merge(x = total, y = materia2,
                by.x = "sigla", by.y = "legislator_state",
                duplicateGeoms = TRUE)
head(total2@data)

# gerando camadas sim e nao
sim <- total
nao <- total

# total
tmap_mode(mode = 'plot')
tm_shape(total) + 
  tm_fill(col = "total", 
          title = paste("Saldo da\n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) 

# sim
tm_shape(sim) + 
  tm_fill(col = "sim", 
          title = paste("Votação a favor da\n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) 

# nao
tm_shape(nao) + 
  tm_fill(col = "nao", 
          title = paste("Votação contra a\n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) 

# modo intereativo
# total
tmap_mode(mode = 'view')
tm_shape(total) + 
  tm_fill(col = "total", 
          title = paste("Votação da\n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) 

# sim
tm_shape(sim) + 
  tm_fill(col = "sim", midpoint = 0,
          title = paste("Votação \n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) 

# nao
tm_shape(nao) + 
  tm_fill(col = "nao", midpoint = 0,
          title = paste("Votação \n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) 

# várias camadas(?)
tm_basemap(server = "CartoDB.PositronNoLabels") +
tm_shape(shp = total, name = "Saldo da votação") +
  # total
  tm_fill(col = "total", 
          title = paste("Votação da\n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  # sim
  tm_shape(shp = sim, name = "Votos a favor") +
  tm_fill(col = "sim", midpoint = 0,
          title = paste("Votação \n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  # nao
  tm_shape(shp = nao, name = "Votos contra") +
  tm_fill(col = "nao", midpoint = 0,
          title = paste("Votação \n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) +
  tm_view(leaflet.options = c(dragging = FALSE), 
          set.zoom.limits = c(3, 6)) 

# salvando os mapas em modo interativo
tmap_save(tm_basemap(server = "CartoDB.PositronNoLabels") +
            # nao
            tm_shape(shp = nao, name = "Votos contra") +
            tm_fill(col = "nao", midpoint = 0,
                    title = paste("Votação \n", resultado_materia_estado1$rollcall_id[1])) +
            tm_borders() +
            tm_text(text = "sigla", size = 1) +
            tm_symbols(alpha = 0, border.alpha = 0,
                       popup.vars = c(Contra = "nao",
                                      Saldo = "total")) +
            # sim
            tm_shape(shp = sim, name = "Votos a favor") +
            tm_fill(col = "sim", midpoint = 0,
                    title = paste("Votação \n", resultado_materia_estado1$rollcall_id[1])) +
            tm_borders() +
            tm_text(text = "sigla", size = 1) +
            tm_symbols(alpha = 0, border.alpha = 0,
                       popup.vars = c(`A Favor` = "sim",
                                      Saldo = "total")) +
            # total
            tm_shape(shp = total, name = "Saldo da votação") +
            tm_fill(col = "total", 
                    title = paste("Votação da\n", resultado_materia_estado1$rollcall_id[1])) +
            tm_borders() +
            tm_text(text = "sigla", size = 1) +
            tm_symbols(alpha = 0, border.alpha = 0,
                       popup.vars = c(Saldo = "total", 
                                      `A Favor` = "sim",
                                      Contra = "nao")) +
            # Região
            tm_shape(regiao) +
            tm_borders(col = "black", lwd = 2) +
            tm_view(leaflet.options = c(dragging = FALSE), 
                    set.zoom.limits = c(4, 10)), "MPV-660-2014-1.html", 
          width = 4, height = 7)  

# Para centralizar a vizualização mapa no Brasil
# library(ggmap)
# geocode("Brazil")
# Source : https://maps.googleapis.com/maps/api/geocode/json?address=Brazil&key=xxx-sI-RxaXqUXt0PCBG9LbcA0a8VE
# A tibble: 1 x 2
#     lon   lat
#   <dbl> <dbl>
# 1 -51.9 -14.2

# Versão dos mapas utilizando diferentes camadas (nao, sim, saldo e partido)  -------------------------------------------

# Carregar pacotes
library(tidyverse)
library(sp)
library(rgdal)
library(tmap)

# Carregar dados  -------------------------------------------
regiao <- readOGR(dsn = "datasets/regioes_2010", layer = "regioes_2010")
estados <- readOGR(dsn = "datasets/estados_2010", layer = "estados_2010")
cham_15 <- read.csv("datasets/cham_2015.csv")

# Gerar tabelas  -------------------------------------------
# Votação por estado
votacao_estado <- cham_15 %>% 
  filter(rollcall_id == levels(factor(cham_15$rollcall_id))[1]) %>% 
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

# Votação por partido por estado
votacao_partido <- cham_15 %>% 
  filter(rollcall_id == levels(factor(cham_15$rollcall_id))[1]) %>% 
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
camada_estado <- merge(x = estados, y = votacao_estado,
                       by.x = "sigla", by.y = "legislator_state", 
                       duplicateGeoms = TRUE) # to merge a spatial object to a data frame

camada_partido <- merge(x = estados, y = votacao_partido,
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
partidos_saldo <- list()

for (i in 1:index){
  # saldo da votação por partido por estado
  partidos_saldo[[i]] <- camada_partido[camada_partido$legislator_party == levels(factor(camada_partido$legislator_party))[i],
                                        c("rollcall_id", "sigla", "legislator_party", "nao", "sim", "saldo")]
  names(partidos_saldo)[[i]] <- levels(factor(camada_partido@data$legislator_party))[i]
}

# Mapa com tmap  -------------------------------------------
tmap_mode("view")

# Mapa com resutlado da votação por estado  -------------------------------------------
popup_vars <- c(`Votos contra: ` = names(estado)[1],
                `Votos a favor: ` = names(estado)[2],
                `Saldo da votação: ` = names(estado)[3])
nomes <- c("Votos contra", "Votos a favor", "Saldo da votação")
posicao1 <- c("right", "right", "right")
posicao2 <- c("top", "bottom", "bottom")
mapa_estado <- list()
mapa_estados_final <- 0
for (i in 1:3){
  mapa_estado_final <- tm_basemap(server = "CartoDB.PositronNoLabels") +
    # sim, não e saldo da votação por estado
    tm_shape(shp = estado[[i]], name = nomes[i]) +
    tm_fill(col = names(estado[[i]])[3], midpoint = 0,
            title = paste0(estado[[i]]$rollcall_id[1], 
                           "\n (",  nomes[i], ")")) +
    tm_borders() +
    tm_text(text = "sigla", size = 1) +
    tm_symbols(alpha = 0, border.alpha = 0,
               popup.vars = popup_vars[i]) +
    tm_view(view.legend.position = c(posicao1[i], posicao2[i]))
  
  mapa_estados_final <- mapa_estados_final + mapa_estado_final
}

mapa_estados_final <- mapa_estados_final +
  # Região
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) +
  tm_view(leaflet.options = c(dragging = FALSE), 
          set.zoom.limits = c(4, 10)) 

mapa_estados_final

# Mapa com resultado da votação por partido por estado --------------------
mapa_partido <- list()
mapa_partidos_final <- 0

for (i in 1:index) {
  mapa_partido[[i]] <- tm_basemap(server = "CartoDB.PositronNoLabels") +
    # saldo por  partido por estado
    tm_shape(shp = partidos_saldo[[i]], name = paste0("Saldo da votação: ", partidos_saldo[[i]]$legislator_party[1])) +
    tm_fill(col = "saldo", 
            title = paste0(partidos_saldo[[i]]$legislator_party[1], " (", partidos_saldo[[i]]$rollcall_id, ")")) +
    tm_borders() +
    tm_text(text = "sigla", size = 1) +
    tm_symbols(alpha = 0, border.alpha = 0,
               popup.vars = c(`Saldo da votação: ` = "saldo",
                              `Votos a favor: ` = "sim",
                              `Votos contra: ` = "nao")) +
    tm_view(leaflet.options = c(dragging = FALSE), 
            set.zoom.limits = c(4, 10), view.legend.position = c("right", "bottom"))
  mapa_partidos_final <- mapa_partidos_final + mapa_partido[[i]]
}

mapa_partidos_final <- mapa_partidos_final +
  # Região
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2)  

mapa_partidos_final

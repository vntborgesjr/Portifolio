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


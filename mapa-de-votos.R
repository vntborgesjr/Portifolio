# --------------------------------------------------- 
# Mapa de votos 
# 09 nov 2020 
# VNTBJR 
# --------------------------------------------------- 
#
# Carregar pacotes
library(congressbr) # o pacote congressbr é um cliente API desenhado para baixar os
# dados diretamente do site da camara dos deputados 
library(tidyverse) # pacote para manipulação, transformação e vizualição dos dados
library(rgdal) # pacote para carregar objetos espaciais
library(sp) # pacote para manipular e visualizar objetos espaciais
library(tmap) # pacote para geração de mapas

# Carregar dados
cham_2015 <- cham_votes_year('2015')
cham_2016 <- cham_votes_year('2016')
cham_2017 <- cham_votes_year('2017')
cham_2018 <- cham_votes_year('2018')
cham_2019 <- cham_votes_year('2019')
cham_2020 <- cham_votes_year('2020')

str(cham_2015)
str(cham_2016)
str(cham_2017)
str(cham_2018)
str(cham_2019)
str(cham_2020)

# Variaveis de interesse
# $rollcall_id
# $decision_summary
# $decision_date
# $rollcall_subject
# $session_id
# $legislator_id
# $legislator_name
# $legislator_state
# $legislator_party
# $legislator_vote
# $number_bill
# $year_bill
# $ano

# gerar conjunto de dados com as variaveis de interesse para cada ano
# 2015
cham_15 <- data.frame(rollcall_id = cham_2015$rollcall_id,
                      decision_summary = cham_2015$decision_summary,
                      decision_date = cham_2015$decision_date,
                      rollcall_subject = cham_2015$rollcall_subject,
                      session_id = cham_2015$session_id,
                      legislator_id = cham_2015$legislator_id,
                      legislator_name = cham_2015$legislator_name,
                      legislator_state = cham_2015$legislator_state,
                      legislator_party = cham_2015$legislator_party,
                      legislator_vote = cham_2015$legislator_vote,
                      number_bill = cham_2015$number_bill,
                      year_bill = cham_2015$year_bill,
                      ano = cham_2015$ano)

# 2016
cham_16 <- data.frame(rollcall_id = cham_2016$rollcall_id,
                      decision_summary = cham_2016$decision_summary,
                      decision_date = cham_2016$decision_date,
                      rollcall_subject = cham_2016$rollcall_subject,
                      session_id = cham_2016$session_id,
                      legislator_id = cham_2016$legislator_id,
                      legislator_name = cham_2016$legislator_name,
                      legislator_state = cham_2016$legislator_state,
                      legislator_party = cham_2016$legislator_party,
                      legislator_vote = cham_2016$legislator_vote,
                      number_bill = cham_2016$number_bill,
                      year_bill = cham_2016$year_bill,
                      ano = cham_2016$ano)

# 2017
cham_17 <- data.frame(rollcall_id = cham_2017$rollcall_id,
                      decision_summary = cham_2017$decision_summary,
                      decision_date = cham_2017$decision_date,
                      rollcall_subject = cham_2017$rollcall_subject,
                      session_id = cham_2017$session_id,
                      legislator_id = cham_2017$legislator_id,
                      legislator_name = cham_2017$legislator_name,
                      legislator_state = cham_2017$legislator_state,
                      legislator_party = cham_2017$legislator_party,
                      legislator_vote = cham_2017$legislator_vote,
                      number_bill = cham_2017$number_bill,
                      year_bill = cham_2017$year_bill,
                      ano = cham_2017$ano)

# 2018
cham_18 <- data.frame(rollcall_id = cham_2018$rollcall_id,
                      decision_summary = cham_2018$decision_summary,
                      decision_date = cham_2018$decision_date,
                      rollcall_subject = cham_2018$rollcall_subject,
                      session_id = cham_2018$session_id,
                      legislator_id = cham_2018$legislator_id,
                      legislator_name = cham_2018$legislator_name,
                      legislator_state = cham_2018$legislator_state,
                      legislator_party = cham_2018$legislator_party,
                      legislator_vote = cham_2018$legislator_vote,
                      number_bill = cham_2018$number_bill,
                      year_bill = cham_2018$year_bill,
                      ano = cham_2018$ano)

# 2019
cham_19 <- data.frame(rollcall_id = cham_2019$rollcall_id,
                      decision_summary = cham_2019$decision_summary,
                      decision_date = cham_2019$decision_date,
                      rollcall_subject = cham_2019$rollcall_subject,
                      session_id = cham_2019$session_id,
                      legislator_id = cham_2019$legislator_id,
                      legislator_name = cham_2019$legislator_name,
                      legislator_state = cham_2019$legislator_state,
                      legislator_party = cham_2019$legislator_party,
                      legislator_vote = cham_2019$legislator_vote,
                      number_bill = cham_2019$number_bill,
                      year_bill = cham_2019$year_bill,
                      ano = cham_2019$ano)

# 2020
cham_20 <- data.frame(rollcall_id = cham_2020$rollcall_id,
                      decision_summary = cham_2020$decision_summary,
                      decision_date = cham_2020$decision_date,
                      rollcall_subject = cham_2020$rollcall_subject,
                      session_id = cham_2020$session_id,
                      legislator_id = cham_2020$legislator_id,
                      legislator_name = cham_2020$legislator_name,
                      legislator_state = cham_2020$legislator_state,
                      legislator_party = cham_2020$legislator_party,
                      legislator_vote = cham_2020$legislator_vote,
                      number_bill = cham_2020$number_bill,
                      year_bill = cham_2020$year_bill,
                      ano = cham_2020$ano)

# Gerando uma unica tabela de dados para o periodo da ultima legisltura mais a 
# legislatura vigente (2015 - 2020)
cham_vote_15_20 <- rbind(cham_15, cham_16, cham_17, cham_18, cham_19, cham_20)
str(cham_vote_15_20)

# Resultado por matéria
resultado_materia <- cham_vote_15_20 %>% 
  group_by(rollcall_id) %>% 
  mutate(vote = ifelse(legislator_vote == "Sim", 1, 
                       ifelse(legislator_vote == "Nao", 1, 
                              ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  group_by(rollcall_id, legislator_vote) %>% 
  filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  summarise(vote = sum(vote))

# Exemplo de gráfico que será gerado pela tabela
ggplot(resultado_materia[resultado_materia$rollcall_id == resultado_materia$rollcall_id[1], ], 
       aes(x = legislator_vote, y = vote)) +
  geom_col() +
  facet_wrap(facets = ~ rollcall_id)

ggplot(resultado_materia[resultado_materia$rollcall_id == resultado_materia$rollcall_id[1], ], 
       aes(x = "", y = vote, fill = legislator_vote)) +
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

# Resultado por matéria e por partido
resultado_materia_partido <- cham_vote_15_20 %>% 
  group_by(rollcall_id, legislator_party) %>% 
  mutate(vote = ifelse(legislator_vote == "Sim", 1, 
                       ifelse(legislator_vote == "Nao", 1, 
                              ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  group_by(rollcall_id, legislator_party, legislator_vote) %>% 
  filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  summarise(vote = sum(vote))

# Exemplo de gráfico que será gerado pela tabela
ggplot(resultado_materia_partido[resultado_materia_partido$rollcall_id == resultado_materia_partido$rollcall_id[1], ], 
       aes(x = legislator_vote, y = vote)) +
  geom_col() +
  facet_wrap(facets = ~ legislator_party)

# Resultado por matéria e por estado
resultado_materia_estado <- cham_vote_15_20 %>% 
  group_by(rollcall_id, legislator_state) %>% 
  mutate(vote = ifelse(legislator_vote == "Sim", 1, 
                       ifelse(legislator_vote == "Nao", -1, 
                              ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
  group_by(rollcall_id, legislator_state, legislator_vote) %>% 
  filter(legislator_vote %in% c("Sim", "Nao")) %>% 
  summarise(vote = sum(vote))

# Alternativa
resultado_materia_estado1 <- cham_vote_15_20 %>% 
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
  summarise(total = sum(total), sim = sum(sim), nao = sum(nao))
head(resultado_materia_estado1)
# Exemplo de gráfico que será gerado pela tabela
ggplot(resultado_materia_estado[resultado_materia_estado$rollcall_id == resultado_materia_estado$rollcall_id[1], ],
       aes(x = legislator_vote, y = vote)) +
  geom_col() +
  facet_wrap(facets = ~ legislator_state)

# Resultado por matéria, por estado e por partido
resultado_materia_estado_partido <- cham_vote_15_20 %>% 
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
head(resultado_materia_estado_partido)
# Exemplo de gráfico que será gerado pela tabela
ggplot(resultado_materia_estado_partido[resultado_materia_estado_partido$rollcall_id == resultado_materia_estado_partido$rollcall_id[1], ],
       aes(x = legislator_vote, y = vote)) +
  geom_col() +
  facet_grid(legislator_party ~ legislator_state)

# Mapa espacial de votação gerado pelo método PCA  -------------------------------------------
# Dados para PCA - Resultado por matéria e por partido
# Como montar essa planilha?????
mapa_materia_partido <- cham_vote_15_20 %>% 
  group_by(rollcall_id, legislator_party, ano) %>% 
  mutate(vote = ifelse(legislator_vote == "Sim", 1, 
                       ifelse(legislator_vote == "Nao", - 1, 
                              ifelse(legislator_vote == "Art. 17", 0,
                                     ifelse(is.na(legislator_vote), 0, 0))))) 

dado15 <- mapa_materia_partido %>% 
  filter(ano == 2015) 
dado15$rollcall_id
dado16 <- mapa_materia_partido %>% 
  filter(ano == 2016) 
dado17 <- mapa_materia_partido %>% 
  filter(ano == 2017) 
dado18 <- mapa_materia_partido %>% 
  filter(ano == 2018) 
dado19 <- mapa_materia_partido %>% 
  filter(ano == 2019) 
dado20 <- mapa_materia_partido %>% 
  filter(ano == 2020) 
partido1 <- merge(dado15, dado16, by = "legislator_party")
str(dado15)
partido <- data.frame(voto2015 = dado15$vote, voto16 = dado16$vote, 
                      voto17 = dado17$vote, voto18 = dado18$vote,
                      voto19 = dado19$vote, voto20 = dado20$vote)

# Construindo os mapas  -------------------------------------------
brasil <- readOGR(dsn = "br_unidades_da_federacao", layer = "BR_UF_2019")
regiao <- readOGR(dsn = "regioes_2010", layer = "regioes_2010")
estados <- readOGR(dsn = "estados_2010", layer = "estados_2010")

summary(brasil)
summary(regiao)
summary(estados)

materia1 <- resultado_materia_estado1[resultado_materia_estado1$rollcall_id == resultado_materia_estado1$rollcall_id[1], ]
head(materia1)

teste1 <- merge(x = estados, y = materia1,
                by.x = "sigla", by.y = "legislator_state", 
                duplicateGeoms = TRUE) # to merge a spatial object to a data frame
head(teste1@data)

tmap_mode(mode = 'plot')
tm_shape(teste1) + 
  tm_fill(col = "total", midpoint = NA, 
          title = paste("Votação \n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) 

tmap_mode(mode = 'view')
tm_shape(teste1) + 
  tm_fill(col = "total", midpoint = NA, 
          title = paste("Votação \n", resultado_materia_estado1$rollcall_id[1])) +
  tm_borders() +
  tm_text(text = "sigla", size = 0.5) +
  tm_shape(regiao) +
  tm_borders(col = "black", lwd = 2) 

tmap_save(tm_shape(teste1) + 
            tm_fill(col = "total", midpoint = NA, 
                    title = paste("Votação \n", resultado_materia_estado1$rollcall_id[1])) +
            tm_borders() +
            tm_text(text = "sigla", size = 0.5) +
            tm_shape(regiao) +
            tm_borders(col = "black", lwd = 2), "MPV-660-2014-1.html", 
          width = 4, height = 7)  
  
  
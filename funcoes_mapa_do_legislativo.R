# --------------------------------------------------- 
# Funções para o projeto mapa de votação do legislativo 
# 13 dez 2020 
# VNTBJR 
# --------------------------------------------------- 
#
###################################################################### 
# selecionar_var_materias:  
# Description: retorna um data frame contendo somente as variáveis essenciais para 
# as análises
# dados: data frame obtido a partir da função cham_year_vote()
######################################################################
selecionar_var_materias <- function(dados) {
  library(dplyr)
  var_selecionadas <- dados %>% 
    select(rollcall_id, decision_summary, decision_date, legislator_name, 
           legislator_state, legislator_party, legislator_vote, 
           bill_number = number_bill, ano)
  return(var_selecionadas)
}
######################################################################

###################################################################### 
# carregar_info_materias:  
# Description: retorna um data frame com as informações sobre as matérias
# desejadas  
# lista_bruta: recebe a coluna rollcall_id obtida a partir da função
# cham_year_vote(), vetor em que cada valor contem o tipo, número, 
# ano e turno da votação da matéria. 
######################################################################
carregar_info_materias <- function(lista_bruta) {
  # Carregar pacotes
  library(stringr)
  library(congressbr)
  library(dplyr)
  
  # Matérias votadas no ano (excluindo os turnos) 
  ids <- unique(str_split(lista_bruta, 
                          pattern = fixed("-"), 
                          simplify = TRUE)[, -4])
  
  # Carregar dados sobre as materias
  info_ls <- mapply(cham_bill_info, ids[, 1], 
                    as.integer(ids[, 2]),
                    as.integer(ids[, 3]), SIMPLIFY = FALSE)
  
  # transformar os dados de lista para data frame
  # primeira etapa é transforma-lo em matrix
  # criar uma matrix para armazenar os dados das matérias
  m <- matrix(nrow = nrow(ids), 
              ncol = ncol(info_ls[[1]]), 
              byrow = TRUE)
  
  # loop para adicionar os dados das matérias linha por linha
  for (i in 1:nrow(ids)) {
    m[i, ] <- as.matrix(info_ls[[i]][1, ], )
  }
  
  # transformar a matriz em data frame e nomear as colunas
  df <- data.frame(m)
  names(df) <- names(info_ls[[1]][1, ])
  
  # selecionar as variáveis de interesse
  df <- df %>% 
    select(bill_number, bill_name, bill_subject, ementa_txt, 
           ementa_explanation, author_name, author_state, 
           author_party)
  return(df)
}
######################################################################

###################################################################### 
# unir_anos:  
# Description: retorna um data frame consolidando os dados de diferentes anos
# dados_ano1: recebe um data frame com os dados do primeiro ano
# ...:  recebe data frame de tantos anos quanto forem necessários para consolidar
######################################################################
unir_anos <- function(dados_ano1, ...) {
  anos_consolidados <- rbind(dados_ano1, ...)
  return(anos_consolidados)
}
######################################################################

###################################################################### 
# resultado_por_materia:  
# Description: gera um data frame contendo o número de votos a favor
# (sim) e contra (não) uma matéria
# dados: recebe um data frame contendo o resultado da função unir_dados()
######################################################################
resultado_por_materia <- function(dados) {
  resultado_materia <- dados %>% 
    group_by(rollcall_id) %>% 
    mutate(vote = ifelse(legislator_vote == "Sim", 1, 
                         ifelse(legislator_vote == "Nao", 1, 
                                ifelse(legislator_vote == "Art. 17", 0, 0)))) %>% 
    group_by(rollcall_id, legislator_vote) %>% 
    filter(legislator_vote %in% c("Sim", "Nao")) %>% 
    summarise(vote = sum(vote))
}
######################################################################

###################################################################### 
# grafico_resultado_por_materia:  
# Description: gera um gráfico de pizza contendo o número de votos
# a favor (sim) e contra uma matéria
# dados: um data frame resultante da função resultado_por_matéria()
# materia1: um inteiro impar que indica a linha da matéria
# materia2: um inteiro par seguinte ao indicado em materia1
######################################################################
grafico_resultado_por_materia <- function(dados, materia1, materia2) {
  resultado_materia %>% 
    filter(rollcall_id == resultado_materia$rollcall_id[materia1]) %>% 
    ggplot(aes(x = "", y = vote, fill = legislator_vote)) +
    geom_bar(colour = "black", stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    ggtitle(ifelse(resultado_materia$vote[materia1] < resultado_materia$vote[materia2],
                   paste("Situação da ", 
                         resultado_materia$rollcall_id[materia1], "\nAprovada"), 
                   paste("Situação da ", 
                         resultado_materia$rollcall_id[materia1], "\nReprovada"))) +
    theme_minimal() + 
    theme(line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(size = 12, 
                                    face = "bold", 
                                    hjust = 0.5))
}
######################################################################

###################################################################### 
# resultado_por_estado:  
# Description: gera um data frame contendo o número de votos a favor
# (sim) e contra (não) uma matéria por estado
# dados: recebe um data frame contendo o resultado da função unir_dados()
######################################################################
resultado_por_estado <- function(dados) {
  estado_votos <- dados %>% 
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
  return(estado_votos)
}
######################################################################

###################################################################### 
# resultado_por_estado_partido:  
# Description: gera um data frame contendo o número de votos a favor
# (sim) e contra (não) uma matéria por estado e por partido   
# dados: recebe um data frame contendo o resultado da função unir_dados()
######################################################################
resultado_por_estado_partido <- function(dados) {
  resultado_materia_estado_partido <- dados %>% 
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
  return(resultado_materia_estado_partido)
}
######################################################################

############################################################
# Modelos Estatísticos
#
# Objetivo:
# Avaliar a associação entre complexidade institucional
# e presença da Pedagogia da Alternância.
#
# Modelos estimados:
#
# Modelo 1:
# Alternância ~ Complexidade escolar
#
# Modelo 2:
# Alternância ~ Complexidade + Matrículas +
#              Dependência administrativa + Estado
#
# Método:
# Regressão logística binária.
#
# Resultado:
# Estimativa das chances (odds ratio) de uma escola
# apresentar alternância.
############################################################

rm(list=ls())
pacman::p_load(openxlsx,tidyr,dplyr,openxlsx,stats,
               data.table,PNADcIBGE,survey,ROCR,
               convey,magrittr,rstudioapi,psych,
               ggplot2,ggmosaic,rstudioapi,fastDummies,lubridate,data.table,
               mapview,geobr, mirt,kableExtra)
options(encoding = "utf-8")
setwd(dirname(getActiveDocumentContext()$path))

load("educacao_amazonia_TRI2.RData")

####modelosINEP####
# Modelo simples (efeito da complexidade)
###estima a associação entre o índice de complexidade institucional e a oferta 
#de pedagogia da alternância de forma isolada, captando o efeito bruto dessa variável
modelo1 <- glm(
  alternancia_3anos ~ complexidade_escola,
  data = educacao_estrutura_2023,
  family = binomial
)

summary(modelo1)
# Modelo com controles
#inclui mais variáveis permitindo isolar o efeito da complexidade ao comparar 
#escolas com características semelhantes
modelo2 <- glm(
  alternancia_3anos ~ complexidade_escola +
    QT_MAT_BAS +
    factor(TP_DEPENDENCIA) +
    factor(CO_UF),
  data = educacao_estrutura_2023,
  family = binomial
)

summary(modelo2)

#Média de complexidade (comparação direta)
educacao_estrutura_2023 %>%
  group_by(alternancia_3anos) %>%
  summarise(
    media_complexidade = mean(complexidade_escola, na.rm = TRUE),
    mediana_complexidade = median(complexidade_escola, na.rm = TRUE),
    n = n()
  )

#Gráfico de probabilidade
ggplot(educacao_estrutura_2023,
       aes(x = complexidade_escola,
           y = alternancia_3anos)) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"))

#Tabelas descritivas
##minha tabela 1
alternancia <- educacao_estrutura_2023 %>%
  filter(alternancia_3anos == 1)

#Porte
table(educacao_estrutura_2023$porte_cat)

porte_alt <- alternancia %>%
  count(porte_cat) %>%
  mutate(perc = round(n / sum(n) * 100, 1))

porte_alt

#Número de etapas
table(educacao_estrutura_2023$numero_etapas)

etapas_alt_num <- alternancia %>%
  count(numero_etapas) %>%
  mutate(perc = round(n / sum(n) * 100, 1))

etapas_alt_num

#Etapa mais alta
table(educacao_estrutura_2023$etapa_mais_alta)

etapa_alt <- alternancia %>%
  count(etapa_mais_alta) %>%
  mutate(perc = round(n / sum(n) * 100, 1))

etapa_alt

### Turno
table(educacao_estrutura_2023$turno_cat)

turno_total <- educacao_estrutura_2023 %>%
  count(turno_cat) %>%
  mutate(
    perc = round(n / sum(n) * 100, 1)
  )

# apenas alternância
turno_alt <- alternancia %>%
  count(turno_cat) %>%
  mutate(
    perc = round(n / sum(n) * 100, 1)
  )

turno_alt
#Odds ratio
exp(coef(modelo2))
# Intervalo de confiança
exp(confint(modelo2))

#nível de complexidade
complexidade_escola <- fscores(modelo_tri)
nivel_complexidade = ntile(complexidade_escola, 6)

##transformar o coeficiente do  modelo logístico em algo interpretável em termos de “chance”
coef(modelo_tri)

##Complexidade média por grupo
educacao_amazonia %>%
  mutate(alternancia_label = ifelse(alternancia_3anos == 1,
                                    "Com alternância",
                                    "Sem alternância")) %>%
  group_by(alternancia_label) %>%
  summarise(
    media_complexidade = mean(complexidade_escola, na.rm = TRUE)
  )

#alternância aumenta com a complexidade
prop.table(
  table(
    educacao_estrutura_2023$nivel_complexidade,
    educacao_estrutura_2023$alternancia_3anos
  ),
  1
)

##Medir o nivel de complexidade
educacao_estrutura_2023 %>%
  filter(!is.na(nivel_complexidade)) %>%
  group_by(nivel_complexidade) %>%
  summarise(
    taxa_alternancia = mean(alternancia_3anos, na.rm = TRUE)
  )


##Tabela 2 — Chance de uma escola ter alternância por etapa mais alta###
tabela_etapa <- educacao_estrutura_2023 %>%
  filter(!is.na(etapa_mais_alta)) %>%
  group_by(etapa_mais_alta) %>%
  summarise(
    escolas = n(),
    com_alternancia = sum(alternancia_3anos == 1, na.rm = TRUE),
    perc_alternancia = round(com_alternancia / escolas * 100, 2)
  )


#tabela 3 — Porte das escolas com e sem alternância
tabela_porte <- educacao_estrutura_2023 %>%
  group_by(alternancia_3anos) %>%
  summarise(
    escolas = n(),
    media_matriculas = round(mean(QT_MAT_BAS, na.rm = TRUE), 0),
    mediana_matriculas = median(QT_MAT_BAS, na.rm = TRUE)
  )


####gráfico- Figura 1: de Distribuição da complexidade escolar
ggplot(
  educacao_estrutura_2023 %>% 
    filter(!is.na(complexidade_escola)),
  aes(
    x = complexidade_escola,
    fill = factor(alternancia_3anos)
  )
) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Índice de complexidade escolar",
    y = "Densidade",
    fill = "Alternância"
  ) +
  scale_fill_manual(
    values = c("0" = "grey70", "1" = "#2A9D8F"),
    labels = c("Sem alternância", "Com alternância")
  ) +
  theme_minimal()


  ####criação da tabela 4 para os níveis de complexidade
#Extrair categoria dominante (50%)
# -------------------------------------------------
categoria_dominante <- function(x, corte = 0.5) {
  
  x <- x[!is.na(x)]  # remove NA
  
  if(length(x) == 0) return(NA_character_)
  
  tab <- prop.table(table(x))
  
  dominante <- tab[tab >= corte]
  
  if(length(dominante) == 0) {
    return(NA_character_)
  } else {
    return(names(dominante)[which.max(dominante)])
  }
}

#-------------------------------------------------
#Construir tabela de perfis por nível
#-------------------------------------------------

tabela_perfis <- educacao_estrutura_2023 %>%
  filter(!is.na(nivel_complexidade)) %>%
  group_by(nivel_complexidade) %>%
  summarise(
    
    n_escolas = n(),
    
    # categorias dominantes (50%)
    porte_dom = categoria_dominante(porte_cat, 0.5),
    etapas_dom = categoria_dominante(numero_etapas, 0.5),
    etapa_alta_dom = categoria_dominante(etapa_mais_alta, 0.5),
    turno_dom = categoria_dominante(turno_cat, 0.5),
    
    # infraestrutura (proporção)
    prop_energia = mean(energia, na.rm = TRUE),
    prop_banheiro = mean(banheiro, na.rm = TRUE),
    prop_biblioteca = mean(biblioteca, na.rm = TRUE),
    prop_cozinha = mean(cozinha, na.rm = TRUE),
    
    .groups = "drop"
  )

#-------------------------------------------------
#Infraestrutura dominante (2/3)
#-------------------------------------------------

tabela_perfis <- tabela_perfis %>%
  mutate(
    energia_dom = ifelse(prop_energia >= 0.667, "Sim", "Não"),
    banheiro_dom = ifelse(prop_banheiro >= 0.667, "Sim", "Não"),
    biblioteca_dom = ifelse(prop_biblioteca >= 0.667, "Sim", "Não"),
    cozinha_dom = ifelse(prop_cozinha >= 0.667, "Sim", "Não")
  )

#-------------------------------------------------
#  Converter categorias (IMPORTANTÍSSIMO: são character!)
#-------------------------------------------------

tabela_perfis <- tabela_perfis %>%
  mutate(
    porte_dom = as.numeric(porte_dom),
    etapa_alta_dom = as.numeric(etapa_alta_dom),
    turno_dom = as.numeric(turno_dom)
  )

#-------------------------------------------------
# Traduzir para texto
#-------------------------------------------------

tabela_perfis <- tabela_perfis %>%
  mutate(
    porte_desc = case_when(
      porte_dom == 1 ~ "até 50 matrículas",
      porte_dom == 2 ~ "50 a 149 matrículas",
      porte_dom == 3 ~ "150 a 299 matrículas",
      porte_dom == 4 ~ "300 a 499 matrículas",
      porte_dom == 5 ~ "500 a 999 matrículas",
      porte_dom == 6 ~ "1000 ou mais",
      TRUE ~ "sem predominância"
    ),
    
    etapa_desc = case_when(
      etapa_alta_dom == 1 ~ "Educação Infantil",
      etapa_alta_dom == 2 ~ "Ensino Fundamental",
      etapa_alta_dom == 3 ~ "Ensino Médio",
      etapa_alta_dom == 4 ~ "EJA",
      TRUE ~ "sem predominância"
    ),
    
    turno_desc = case_when(
      turno_dom == 1 ~ "1 turno",
      turno_dom == 2 ~ "2 turnos",
      turno_dom == 3 ~ "3 turnos ou mais",
      TRUE ~ "sem predominância"
    )
  )

#-------------------------------------------------
#  Visualizar resultado
#-------------------------------------------------

tabela_perfis

#Gráfico principal: taxa de alternância por nível de complexidade
dados_graf <- educacao_estrutura_2023 %>%
  group_by(nivel_complexidade) %>%
  summarise(
    taxa_alternancia = mean(alternancia_3anos, na.rm = TRUE)*100,
    n = n()
  )

#
ggplot(
  educacao_estrutura_2023,
  aes(
    x = complexidade_escola,
    y = factor(nivel_complexidade),
    fill = factor(nivel_complexidade)
  )
) +
  ggridges::geom_density_ridges(
    alpha = .8
  )


#
tab <- educacao_estrutura_2023 %>%
  count(SG_UF, nivel_complexidade)

ggplot(
  tab,
  aes(
    x = factor(nivel_complexidade),
    y = SG_UF,
    fill = n
  )
) +
  geom_tile()



#
ggplot(
  dados_graf,
  aes(
    x = factor(nivel_complexidade),
    y = taxa_alternancia*100,
    fill = factor(nivel_complexidade)
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(taxa_alternancia*100,2), "%")),
    vjust = -0.3
  )



#taxa de alternância dentro de cada nível
alternancia <- educacao_estrutura_2023 %>%
  filter(alternancia_3anos == 1)

alternancia %>%
  count(nivel_complexidade)


#níveis estão as escolas de alternância
ggplot(
  alternancia %>%
    count(nivel_complexidade),
  aes(
    x = factor(nivel_complexidade),
    y = n
  )
) +
  geom_col()


#
educacao_estrutura_2023 %>%
  filter(alternancia_3anos == 1) %>%
  count(nivel_complexidade) %>%
  mutate(
    perc = n/sum(n)*100
  )


#
alternancia %>%
  filter(!is.na(nivel_complexidade)) %>%
  count(nivel_complexidade) %>%
  ggplot(
    aes(
      x = factor(nivel_complexidade),
      y = n,
      fill = factor(nivel_complexidade)
    )
  ) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "1" = "#FFFFCC",
      "2" = "#FFEDA0",
      "3" = "#FED976",
      "4" = "#FEB24C",
      "5" = "#F03B20",
      "6" = "#BD0026"
    ),
    name = "Nível de Complexidade"
  ) +
  labs(
    x = "Nível de complexidade institucional",
    y = "Número de escolas de alternância"
  ) +
  theme_minimal()


#taxa de alternância por nível
dados_taxa <-educacao_estrutura_2023 %>%
  filter(!is.na(nivel_complexidade)) %>%
  group_by(nivel_complexidade) %>%
  summarise(
    total_escolas = n(),
    escolas_alternancia = sum(alternancia_3anos == 1),
    taxa_alternancia = escolas_alternancia / total_escolas * 100
  )


#gráfico
ggplot(
  dados_taxa,
  aes(
    x = factor(nivel_complexidade),
    y = taxa_alternancia,
    fill = factor(nivel_complexidade)
  )
) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "1" = "#FFFFCC",
      "2" = "#FFEDA0",
      "3" = "#FED976",
      "4" = "#FEB24C",
      "5" = "#F03B20",
      "6" = "#BD0026"
    ),
    name = "Nível de Complexidade"
  ) +
  geom_text(
    aes(label = paste0(round(taxa_alternancia, 2), "%")),
    vjust = -0.3
  )+
  geom_text(
    aes(label = paste0(round(taxa_alternancia, 2), "%")),
    vjust = -0.3
  )+
  labs(
    x = "Nível de complexidade institucional",
    y = "Taxa de alternância (%)"
  ) +
  theme_minimal()


#salvando
ggsave(
  "Taxa de alternância.png",
  width = 8,
  height = 7,
  dpi = 300
)



#conferindo a quantidade de escolas por nível que estava estranho
#ta certo então fica como tava

nrow(educacao_estrutura_2023)

#
n_distinct(educacao_estrutura_2023$CO_ENTIDADE)

#
table(educacao_estrutura_2023$nivel_complexidade)

educacao_estrutura_2023 %>%
  group_by(nivel_complexidade) %>%
  summarise(
    total_escolas = n_distinct(CO_ENTIDADE),
    escolas_alternancia = sum(IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
  )


educacao_estrutura_2023$complexidade_escola
educacao_estrutura_2023$nivel_complexidade
ntile(complexidade_escola, 6)

summary(educacao_estrutura_2023$complexidade_escola)
table(educacao_estrutura_2023$nivel_complexidade, useNA = "always")


############################################################
# Tabelas e Figuras dos Resultados
#
# Objetivo:
# Produzir as tabelas descritivas, indicadores de probabilidade
# e representações gráficas utilizadas na análise da
# Pedagogia da Alternância.
#
# Fonte:
# Censo Escolar/INEP (2023)
############################################################

rm(list=ls())
pacman::p_load(openxlsx,tidyr,dplyr,openxlsx,stats,
               data.table,PNADcIBGE,survey,ROCR,
               convey,magrittr,rstudioapi,psych,
               ggplot2,ggmosaic,rstudioapi,fastDummies,lubridate,data.table,
               mapview,geobr, mirt,kableExtra, ggridges)
options(encoding = "utf-8")
setwd(dirname(getActiveDocumentContext()$path))

load("educacao_amazonia_TRI2.RData")

############################################################
# TABELA 1
# Distribuição das escolas rurais segundo características
# institucionais
############################################################


# Criar base apenas com alternância

alternancia <- educacao_estrutura_2023 %>%
  filter(alternancia_3anos == 1)



############################################################
# 1. Porte da escola
############################################################


tabela_porte <- educacao_estrutura_2023 %>%
  
  group_by(porte_cat) %>%
  
  summarise(
    
    escolas_rurais = n(),
    
    perc_rurais =
      round(n()/nrow(educacao_estrutura_2023)*100,1),
    
    escolas_alternancia =
      sum(alternancia_3anos == 1),
    
    perc_alternancia =
      round(escolas_alternancia/
              sum(educacao_estrutura_2023$alternancia_3anos == 1)
            *100,1)
  )



############################################################
# 2. Número de etapas ofertadas
############################################################


tabela_etapas <- educacao_estrutura_2023 %>%
  
  group_by(numero_etapas) %>%
  
  summarise(
    
    escolas_rurais = n(),
    
    perc_rurais =
      round(n()/nrow(educacao_estrutura_2023)*100,1),
    
    escolas_alternancia =
      sum(alternancia_3anos == 1),
    
    perc_alternancia =
      round(escolas_alternancia/
              sum(educacao_estrutura_2023$alternancia_3anos == 1)
            *100,1)
    
  )



############################################################
# 3. Etapa mais elevada ofertada
############################################################


tabela_etapa_elevada <- educacao_estrutura_2023 %>%
  
  group_by(etapa_mais_alta) %>%
  
  summarise(
    
    escolas_rurais = n(),
    
    perc_rurais =
      round(n()/nrow(educacao_estrutura_2023)*100,1),
    
    escolas_alternancia =
      sum(alternancia_3anos == 1),
    
    perc_alternancia =
      round(escolas_alternancia/
              sum(educacao_estrutura_2023$alternancia_3anos == 1)
            *100,1)
    
  )



############################################################
# 4. Número de turnos
############################################################


tabela_turnos <- educacao_estrutura_2023 %>%
  
  group_by(turno_cat) %>%
  
  summarise(
    
    escolas_rurais = n(),
    
    perc_rurais =
      round(n()/nrow(educacao_estrutura_2023)*100,1),
    
    escolas_alternancia =
      sum(alternancia_3anos == 1),
    
    perc_alternancia =
      round(escolas_alternancia/
              sum(educacao_estrutura_2023$alternancia_3anos == 1)
            *100,1)
    
  )




############################################################
# TABELA 2
# Probabilidade de ocorrência de alternância segundo
# etapa mais elevada ofertada
############################################################


tabela_probabilidade_etapa <-
  
  educacao_estrutura_2023 %>%
  
  group_by(etapa_mais_alta) %>%
  
  summarise(
    
    total_escolas = n(),
    
    escolas_alternancia =
      sum(alternancia_3anos == 1),
    
    probabilidade =
      round(escolas_alternancia/
              total_escolas*100,2)
  )



############################################################
# COMPLEXIDADE MÉDIA COM E SEM ALTERNÂNCIA
############################################################


tabela_complexidade <- educacao_estrutura_2023 %>%
  
  mutate(
    grupo = ifelse(
      alternancia_3anos == 1,
      "Com alternância",
      "Sem alternância"
    )
  ) %>%
  
  group_by(grupo) %>%
  
  summarise(
    
    media_complexidade =
      round(mean(complexidade_escola,na.rm=TRUE),3),
    
    mediana_complexidade =
      round(median(complexidade_escola,na.rm=TRUE),3),
    
    n = n()
  )



############################################################
# TAXA DE ALTERNÂNCIA POR NÍVEL DE COMPLEXIDADE
############################################################


taxa_complexidade <- educacao_estrutura_2023 %>%
  
  filter(!is.na(nivel_complexidade)) %>%
  
  group_by(nivel_complexidade) %>%
  
  summarise(
    
    total_escolas = n(),
    
    escolas_alternancia =
      sum(alternancia_3anos == 1),
    
    taxa_alternancia =
      round(
        escolas_alternancia/
          total_escolas*100,
        2
      )
  )



############################################################
# FIGURA 1
# MAPA DAS ESCOLAS COM ALTERNÂNCIA
############################################################


alternancia_mapa <- educacao_estrutura_2023 %>%
  filter(alternancia_3anos == 1)


alternancia_mapa <- alternancia_mapa %>%
  rename(code_school = CO_ENTIDADE) %>%
  mutate(
    code_school = as.character(code_school)
  )


escolas_amazonia <- read_schools(
  year = 2023,
  showProgress = TRUE,
  cache = TRUE
) %>%
  
  mutate(
    code_school = as.character(code_school)
  )



final_sf <- escolas_amazonia %>%
  inner_join(
    alternancia_mapa,
    by="code_school"
  )


ufs_amazonia <- c(
  "PA",
  "MT",
  "TO",
  "RO",
  "MA"
)


mapa_ufs <- read_state(year=2020) %>%
  filter(abbrev_state %in% ufs_amazonia)



figura1 <- ggplot()+
  
  geom_sf(
    data=mapa_ufs,
    fill="gray95"
  )+
  
  geom_sf(
    data=final_sf,
    aes(color=factor(nivel_complexidade)),
    size=2
  )+
  
  labs(
    title="Escolas com Pedagogia da Alternância na Amazônia",
    caption="Fonte: INEP (Censo Escolar da Educação Básica)"
  )+
  
  theme_minimal()



ggsave(
  "Figura1_mapa_alternancia.png",
  figura1,
  width=8,
  height=7,
  dpi=300
)



############################################################
# FIGURA 2
# Taxa de alternância por nível de complexidade
############################################################


figura2 <- ggplot(
  taxa_complexidade,
  aes(
    x=factor(nivel_complexidade),
    y=taxa_alternancia
  )
)+
  
  geom_col()+
  
  geom_text(
    aes(label=paste0(taxa_alternancia,"%")),
    vjust=-0.3
  )+
  
  labs(
    x="Nível de complexidade institucional",
    y="Taxa de alternância (%)"
  )+
  
  theme_minimal()



ggsave(
  "Figura2_taxa_alternancia_complexidade.png",
  figura2,
  width=8,
  height=6,
  dpi=300
)




############################################################
# FIGURA 3
# Distribuição do índice de complexidade institucional
############################################################


figura3 <- ggplot(
  
  educacao_estrutura_2023 %>%
    filter(!is.na(complexidade_escola)),
  
  aes(
    x=complexidade_escola,
    fill=factor(alternancia_3anos)
  )
)+
  
  geom_density(alpha=0.4)+
  
  labs(
    x="Índice de complexidade institucional",
    y="Densidade",
    fill="Alternância"
  )+
  
  theme_minimal()



ggsave(
  "Figura3_distribuicao_complexidade.png",
  figura3,
  width=8,
  height=6,
  dpi=300
)



############################################################
# SALVAR RESULTADOS
############################################################


save(
  tabela_porte,
  tabela_etapas,
  tabela_etapa_elevada,
  tabela_turnos,
  tabela_probabilidade_etapa,
  tabela_complexidade,
  taxa_complexidade,
  odds_ratio,
  intervalo_confianca,
  file="Resultados_Tabelas_Figuras.RData"
)

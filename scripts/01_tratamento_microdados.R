############################################################
# Pesquisa:
# Pedagogia da Alternância e Complexidade Institucional
# das Escolas da Amazônia
#
# Objetivo:
# Construir a base histórica das escolas da Amazônia Legal
# utilizando os microdados do Censo Escolar/INEP (2013-2024).
#
# Etapas:
# 1. Importação dos microdados anuais;
# 2. Seleção das variáveis utilizadas na pesquisa;
# 3. Construção da base histórica;
# 4. Identificação da presença persistente da Pedagogia
#    da Alternância;
# 5. Criação da base final para análise.
#
# Fonte:
# Microdados do Censo Escolar - INEP.
############################################################


rm(list=ls())
pacman::p_load(openxlsx,tidyr,dplyr,openxlsx,stats,
               data.table,PNADcIBGE,survey,ROCR,
               convey,magrittr,rstudioapi,psych,
               ggplot2,ggmosaic,rstudioapi,fastDummies,lubridate,data.table,
               mapview,geobr)
options(encoding = "utf-8")
setwd(dirname(getActiveDocumentContext()$path))

#------------------------------------------------------------
# 1. Importação dos microdados
#------------------------------------------------------------
educacao_13 <- fread("microdados_ed_basica_2013.csv")
educacao_14 <- fread("microdados_ed_basica_2014.csv")
educacao_15<- fread("microdados_ed_basica_2015.csv")
educacao_16 <- fread("microdados_ed_basica_2016.csv")
educacao_17 <- fread("microdados_ed_basica_2017.csv")
educacao_18 <- fread("microdados_ed_basica_2018.csv")
educacao_19 <- fread("microdados_ed_basica_2019.csv")
educacao_20 <- fread("microdados_ed_basica_2020.csv")
educacao_21 <- fread("microdados_ed_basica_2021.csv")
educacao_22 <- fread("microdados_ed_basica_2022.csv")
educacao_23 <- fread("microdados_ed_basica_2023.csv")
educacao_24 <- fread("microdados_ed_basica_2024.csv")


#Para ver seVer se a alternância aparece por ANO
sum(educacao_13$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_14$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_15$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_16$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_17$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_18$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_19$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_20$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_21$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_22$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_23$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)
sum(educacao_24$IN_FORMACAO_ALTERNANCIA == 1, na.rm = TRUE)

#nos anos de 22, 23 e 24 deu valor de 0
#por isso vou ver variável só tem 0 e NA em 2022–2024
#em 2023 e 2024 não tá a "IN_FORMACAO_ALTERNANCIA"
unique(educacao_22$IN_FORMACAO_ALTERNANCIA) #todos os valores são NA
unique(educacao_23)
unique(educacao_24)
#Para tentar usar os anos
vars_faltantes <- c(
  "IN_FORMACAO_ALTERNANCIA",
  "IN_AGUA_FILTRADA",
  "QT_SALAS_EXISTENTES",
  "QT_FUNCIONARIOS"
)

educacao_23[, (vars_faltantes) := 0L]
educacao_24[, (vars_faltantes) := 0L]
#------------------------------------------------------------
# 2. Seleção das variáveis utilizadas
#------------------------------------------------------------
#padronizar as variaveis
vars_base <- c(
  "NU_ANO_CENSO",  "CO_ENTIDADE",  "IN_FORMACAO_ALTERNANCIA",
  "CO_UF",  "CO_MUNICIPIO",  "TP_LOCALIZACAO",
  "TP_DEPENDENCIA",  "LATITUDE",
  "LONGITUDE",  "TP_OCUPACAO_PREDIO_ESCOLAR",
  "IN_AGUA_FILTRADA", "IN_AGUA_POTAVEL", "IN_AGUA_REDE_PUBLICA", "IN_AGUA_POCO_ARTESIANO",
  "IN_AGUA_CACIMBA","IN_AGUA_FONTE_RIO", "IN_AGUA_INEXISTENTE", "IN_ENERGIA_REDE_PUBLICA","IN_BANHEIRO",
  "IN_BIBLIOTECA","IN_COZINHA",
  "QT_SALAS_EXISTENTES","QT_FUNCIONARIOS","QT_MAT_INF", "QT_MAT_BAS", "QT_MAT_FUND",
  "QT_MAT_MED", "QT_MAT_EJA","QT_MAT_BAS_FEM","QT_MAT_BAS_ND",  "QT_MAT_BAS_BRANCA",
  "QT_MAT_BAS_PRETA",  "QT_MAT_BAS_PARDA",  "QT_MAT_BAS_AMARELA",  "QT_MAT_BAS_INDIGENA",
  "QT_MAT_BAS_D", "QT_DOC_BAS",  "QT_DOC_INF", "QT_DOC_FUND","QT_DOC_MED", "QT_TUR_BAS",
  "QT_TUR_INF", "QT_TUR_FUND","QT_TUR_MED","QT_TUR_EJA","IN_PROF", "TP_LOCALIZACAO_DIFERENCIADA",
  "QT_DOC_EJA", "IN_DIURNO", "IN_NOTURNO", "IN_EAD"
)

educacao_13 <- educacao_13 %>% select(any_of(vars_base))
educacao_14 <- educacao_14 %>% select(any_of(vars_base))
educacao_15 <- educacao_15 %>% select(any_of(vars_base))
educacao_16 <- educacao_16 %>% select(any_of(vars_base))
educacao_17 <- educacao_17 %>% select(any_of(vars_base))
educacao_18 <- educacao_18 %>% select(any_of(vars_base))
educacao_19 <- educacao_19 %>% select(any_of(vars_base))
educacao_20 <- educacao_20 %>% select(any_of(vars_base))
educacao_21 <- educacao_21 %>% select(any_of(vars_base))
educacao_22 <- educacao_22 %>% select(any_of(vars_base))
educacao_23 <- educacao_23 %>% select(any_of(vars_base))
educacao_24 <- educacao_24 %>% select(any_of(vars_base))


#------------------------------------------------------------
# 3. Construção da base histórica
#------------------------------------------------------------
educacao_total <- dplyr::bind_rows(
  educacao_13, educacao_14, educacao_15, educacao_16,
  educacao_17, educacao_18, educacao_19, educacao_20,
  educacao_21, educacao_22, educacao_23, educacao_24
)

View(educacao_total)
#Defina explicitamente o período “válido” da alternância
anos_validos_alternancia <- 2013:2021

#------------------------------------------------------------
# 4. Seleção territorial
#
# Estados pertencentes à Amazônia Legal:
# AC, AP, AM, MA, MT, PA, RO, RR, TO
#
#------------------------------------------------------------

ufs_amazonia_fronteira <- c(
  15, # Pará
  51, # Mato Grosso
  17, # Tocantins
  11, # Rondônia
  21  # Maranhão
)

#------------------------------------------------------------
# 5. Construção do indicador de alternância persistente
#
# Critério:
# escola considerada com alternância quando apresentou
# registro em pelo menos três anos entre 2013 e 2021.
#
#------------------------------------------------------------


anos_validos_alternancia <- 2013:2021


alternancia_indicadores <- educacao_amazonia %>%
  filter(
    NU_ANO_CENSO %in% anos_validos_alternancia
  ) %>%
  group_by(
    CO_ENTIDADE
  ) %>%
  summarise(

    anos_observados =
      n_distinct(NU_ANO_CENSO),

    anos_alternancia =
      sum(
        IN_FORMACAO_ALTERNANCIA == 1,
        na.rm = TRUE
      ),

    alternancia_3anos =
      as.integer(
        anos_alternancia >= 3
      ),

    .groups = "drop"

  )



#------------------------------------------------------------
# 6. Incorporar indicador na base
#------------------------------------------------------------


educacao_amazonia <- educacao_amazonia %>%

  left_join(
    alternancia_indicadores %>%
      select(
        CO_ENTIDADE,
        alternancia_3anos
      ),
    by = "CO_ENTIDADE"
  ) %>%

  mutate(
    alternancia_3anos =
      ifelse(
        is.na(alternancia_3anos),
        0,
        alternancia_3anos
      )
  )



#------------------------------------------------------------
# 7. Salvar base final
#------------------------------------------------------------


save(
  educacao_amazonia,
  file = "Educacao_Amazonia_Fronteira.RData"
)

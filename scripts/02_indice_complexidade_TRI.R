############################################################
# Construção do Índice de Complexidade Institucional
#
# Objetivo:
# Estimar um indicador sintético de complexidade das escolas
# rurais da Amazônia Legal utilizando Teoria de Resposta ao
# Item (TRI).
#
# Dimensões consideradas:
# - Infraestrutura escolar;
# - Oferta de etapas educacionais;
# - Porte da escola;
# - Organização dos turnos.
#
# Método:
# Modelo TRI ordinal utilizando pacote mirt.
#
# Resultado:
# Índice contínuo de complexidade escolar.
############################################################
rm(list=ls())
pacman::p_load(openxlsx,tidyr,dplyr,openxlsx,stats,
               data.table,PNADcIBGE,survey,ROCR,
               convey,magrittr,rstudioapi,psych,
               ggplot2,ggmosaic,rstudioapi,fastDummies,lubridate,data.table,
               mapview,geobr, mirt, kableExtra)
options(encoding = "utf-8")
setwd(dirname(getActiveDocumentContext()$path))

load("Educacao_Amazonia_Fronteira.RData")


#-------------------------------------------------
# 2. Fotografia estrutural (2023)
#-------------------------------------------------

educacao_estrutura_2023 <- educacao_amazonia %>%
  filter(NU_ANO_CENSO == 2023)

#-------------------------------------------------
# 3. Filtro: escolas rurais
#-------------------------------------------------

educacao_estrutura_2023 <- educacao_estrutura_2023 %>%
  filter(TP_LOCALIZACAO == 2)

#-------------------------------------------------
# 4. Infraestrutura (água detalhada + síntese)
#-------------------------------------------------

educacao_estrutura_2023 <- educacao_estrutura_2023 %>%
  mutate(
    # água detalhada (para análise)
    agua_rede_publica   = as.integer(IN_AGUA_REDE_PUBLICA == 1),
    agua_poco_artesiano = as.integer(IN_AGUA_POCO_ARTESIANO == 1),
    agua_cacimba        = as.integer(IN_AGUA_CACIMBA == 1),
    agua_rio            = as.integer(IN_AGUA_FONTE_RIO == 1),
    
    # variável síntese (para TRI)
    agua_tipo = case_when(
      agua_rede_publica == 1 ~ 4,
      agua_poco_artesiano == 1 ~ 3,
      agua_cacimba == 1 ~ 2,
      agua_rio == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # demais infraestruturas
    energia   = as.integer(IN_ENERGIA_REDE_PUBLICA == 1),
    banheiro  = as.integer(IN_BANHEIRO == 1),
    biblioteca= as.integer(IN_BIBLIOTECA == 1),
    cozinha   = as.integer(IN_COZINHA == 1)
  )
# 4.1 Organização escolar (turnos)
#-------------------------------------------------

educacao_estrutura_2023 <- educacao_estrutura_2023 %>%
  mutate(
    turno_diurno = as.integer(IN_DIURNO == 1),
    turno_noturno = as.integer(IN_NOTURNO == 1),
    
    numero_turnos = turno_diurno + turno_noturno,
    
    turno_cat = case_when(
      numero_turnos == 1 ~ 1,
      numero_turnos == 2 ~ 2,
      numero_turnos >= 3 ~ 3,
      TRUE ~ NA_real_
    )
  )
#-------------------------------------------------
# 5. Porte da escola
#-------------------------------------------------

educacao_estrutura_2023 <- educacao_estrutura_2023 %>%
  mutate(
    porte_cat = case_when(
      QT_MAT_BAS < 50 ~ 1,
      QT_MAT_BAS < 150 ~ 2,
      QT_MAT_BAS < 300 ~ 3,
      QT_MAT_BAS < 500 ~ 4,
      QT_MAT_BAS < 1000 ~ 5,
      TRUE ~ 6
    )
  )

#-------------------------------------------------
# 6. Etapas educacionais
#-------------------------------------------------

educacao_estrutura_2023 <- educacao_estrutura_2023 %>%
  mutate(
    etapa_inf  = as.integer(QT_MAT_INF > 0),
    etapa_fund = as.integer(QT_MAT_FUND > 0),
    etapa_med  = as.integer(QT_MAT_MED > 0),
    etapa_eja  = as.integer(QT_MAT_EJA > 0),
    
    numero_etapas = etapa_inf + etapa_fund + etapa_med + etapa_eja
  ) %>%
  filter(numero_etapas > 0)

# etapa mais alta (ordinal)
educacao_estrutura_2023 <- educacao_estrutura_2023 %>%
  mutate(
    etapa_mais_alta = case_when(
      etapa_eja == 1 ~ 4,
      etapa_med == 1 ~ 3,
      etapa_fund == 1 ~ 2,
      etapa_inf == 1 ~ 1
    )
  )

#-------------------------------------------------
# 7. Base para TRI
#-------------------------------------------------

itens_tri <- educacao_estrutura_2023 %>%
  select(
    agua_tipo,
    energia,
    banheiro,
    biblioteca,
    cozinha,
    porte_cat,
    etapa_mais_alta
  )

# verificar NA
colSums(is.na(itens_tri))

# manter apenas casos completos
dados_tri <- itens_tri %>% na.omit()

#-------------------------------------------------
# 8. Modelo TRI
#-------------------------------------------------

modelo_tri <- mirt(dados_tri, 1, itemtype = "graded")

# parâmetros dos itens
coef(modelo_tri, simplify = TRUE)$items

#-------------------------------------------------
# 9. Escore de complexidade
#-------------------------------------------------

educacao_estrutura_2023$complexidade_escola <- NA

educacao_estrutura_2023$complexidade_escola[
  complete.cases(itens_tri)
] <- fscores(modelo_tri)[,1]

#-------------------------------------------------
# 10. Níveis de complexidade
#-------------------------------------------------

educacao_estrutura_2023 <- educacao_estrutura_2023 %>%
  mutate(
    nivel_complexidade = ntile(complexidade_escola, 6)
  )

#-------------------------------------------------
# 11. Modelo logístico
#-------------------------------------------------

modelo_logit <- glm(
  alternancia_3anos ~ complexidade_escola,
  data = educacao_estrutura_2023,
  family = binomial
)

summary(modelo_logit)

#-------------------------------------------------
# 12. Gráfico (probabilidade prevista)
#-------------------------------------------------

ggplot(educacao_estrutura_2023,
       aes(x = complexidade_escola, y = alternancia_3anos)) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"))

#salvando
save(educacao_estrutura_2023, file = "educacao_amazonia_TRI2.RData")





#Peso relativo do TRI por dimensão
coef_itens <- coef(modelo_tri, simplify = TRUE)$items

pesos <- coef_itens[, "a1"]
pesos_relativos <- pesos / sum(pesos)

round(100 * pesos_relativos, 2)

#quanto cada variável explica da complexidade
coef_itens <- coef(modelo_tri, simplify = TRUE)$items

coef_itens %>%
  as.data.frame() %>%
  mutate(
    carga = a1 / sqrt(1 + a1^2),
    R2 = carga^2
  )


plot(modelo_tri, type = "infotrace")






############################################################
# Pesquisa:
# Pedagogia da Alternância e Complexidade Institucional
# das Escolas da Amazônia
#
# Objetivo:
# Avaliar a associação entre complexidade institucional
# e presença da Pedagogia da Alternância.
#
# Método:
# Regressão logística binária.
#
# Variável dependente:
# Alternancia_3anos
#
# Modelos:
#
# Modelo 1:
# Alternância ~ Complexidade escolar
#
# Modelo 2:
# Alternância ~ Complexidade +
#              Matrículas +
#              Dependência administrativa +
#              Estado
#
# Resultado:
# Odds ratio da probabilidade de uma escola apresentar
# Pedagogia da Alternância.
############################################################


rm(list = ls())


pacman::p_load(
  dplyr,
  ggplot2,
  stats,
  rstudioapi
)


options(encoding = "utf-8")


setwd(dirname(getActiveDocumentContext()$path))


load(
  "educacao_amazonia_TRI.RData"
)



#------------------------------------------------------------
# 1. Modelo logístico simples
#
# Efeito bruto da complexidade institucional
#------------------------------------------------------------


modelo1 <- glm(

  alternancia_3anos ~ complexidade_escola,

  data = educacao_estrutura_2023,

  family = binomial

)


summary(modelo1)



#------------------------------------------------------------
# 2. Modelo logístico com controles
#
# Isola o efeito da complexidade comparando escolas
# com características semelhantes
#------------------------------------------------------------


modelo2 <- glm(

  alternancia_3anos ~

    complexidade_escola +

    QT_MAT_BAS +

    factor(TP_DEPENDENCIA) +

    factor(CO_UF),

  data = educacao_estrutura_2023,

  family = binomial

)


summary(modelo2)



#------------------------------------------------------------
# 3. Odds Ratio
#------------------------------------------------------------


odds_ratio <- exp(
  coef(modelo2)
)


odds_ratio



# Intervalo de confiança

intervalo_or <- exp(
  confint(modelo2)
)


intervalo_or



#------------------------------------------------------------
# 4. Complexidade média por presença de alternância
#------------------------------------------------------------


complexidade_media <- educacao_estrutura_2023 %>%

  group_by(
    alternancia_3anos
  ) %>%

  summarise(

    media_complexidade =
      mean(
        complexidade_escola,
        na.rm = TRUE
      ),

    mediana_complexidade =
      median(
        complexidade_escola,
        na.rm = TRUE
      ),

    numero_escolas = n()

  )


complexidade_media



#------------------------------------------------------------
# 5. Probabilidade estimada de alternância
#------------------------------------------------------------


ggplot(

  educacao_estrutura_2023,

  aes(
    x = complexidade_escola,
    y = alternancia_3anos
  )

) +

geom_smooth(

  method = "glm",

  method.args =
    list(
      family = "binomial"
    )

) +

labs(

  x = "Índice de complexidade institucional",

  y = "Probabilidade de alternância"

) +

theme_minimal()



#------------------------------------------------------------
# 6. Taxa de alternância por nível de complexidade
#------------------------------------------------------------


dados_taxa <- educacao_estrutura_2023 %>%

  filter(
    !is.na(nivel_complexidade)
  ) %>%

  group_by(
    nivel_complexidade
  ) %>%

  summarise(

    total_escolas = n(),

    escolas_alternancia =
      sum(
        alternancia_3anos == 1,
        na.rm = TRUE
      ),

    taxa_alternancia =
      escolas_alternancia /
      total_escolas * 100

  )


dados_taxa



#------------------------------------------------------------
# Gráfico:
# Taxa de alternância por nível
#------------------------------------------------------------


ggplot(

  dados_taxa,

  aes(

    x = factor(nivel_complexidade),

    y = taxa_alternancia

  )

) +

geom_col() +

geom_text(

  aes(
    label =
      paste0(
        round(taxa_alternancia,2),
        "%"
      )
  ),

  vjust = -0.3

) +

labs(

  x = "Nível de complexidade institucional",

  y = "Taxa de alternância (%)"

) +

theme_minimal()



ggsave(

  "Taxa_alternancia_complexidade.png",

  width = 8,

  height = 7,

  dpi = 300

)



#------------------------------------------------------------
# 7. Perfil das escolas por nível de complexidade
#------------------------------------------------------------


categoria_dominante <- function(x){

  x <- x[!is.na(x)]

  tab <- prop.table(table(x))

  names(
    which.max(tab)
  )

}



tabela_perfis <- educacao_estrutura_2023 %>%

  filter(
    !is.na(nivel_complexidade)
  ) %>%

  group_by(
    nivel_complexidade
  ) %>%

  summarise(

    numero_escolas = n(),

    porte_predominante =
      categoria_dominante(
        porte_cat
      ),

    etapa_predominante =
      categoria_dominante(
        etapa_mais_alta
      ),


    energia =
      mean(
        energia,
        na.rm = TRUE
      ),

    banheiro =
      mean(
        banheiro,
        na.rm = TRUE
      ),

    biblioteca =
      mean(
        biblioteca,
        na.rm = TRUE
      ),

    cozinha =
      mean(
        cozinha,
        na.rm = TRUE
      )

  )


tabela_perfis



#------------------------------------------------------------
# 8. Distribuição da complexidade
#------------------------------------------------------------


ggplot(

  educacao_estrutura_2023 %>%

    filter(
      !is.na(complexidade_escola)
    ),

  aes(

    x = complexidade_escola,

    fill =
      factor(alternancia_3anos)

  )

) +

geom_density(
  alpha = 0.4
) +

labs(

  x = "Índice de complexidade institucional",

  y = "Densidade",

  fill = "Alternância"

) +

theme_minimal()

#------------------------------------------------------------
# 9 TABELA DOS MODELOS LOGÍSTICOS
#------------------------------------------------------------

modelo_resultados <- list(
  Modelo 1 = modelo1,
  Modelo 2 = modelo2
)

modelo_resultados


#------------------------------------------------------------
# 9. Salvar resultados
#------------------------------------------------------------


save(

  modelo1,
  modelo2,
  dados_taxa,
  tabela_perfis,

  file =
    "Resultados_modelos_estatisticos.RData"

)

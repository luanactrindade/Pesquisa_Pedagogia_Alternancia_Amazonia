############################################################
# Distribuição espacial das escolas com Pedagogia da Alternância
#
# Objetivo:
# Representar espacialmente as escolas rurais com presença
# persistente da Pedagogia da Alternância no Arco do
# Desmatamento da Amazônia Legal.
#
# Fonte:
# Censo Escolar/INEP (2023) e geobr.
############################################################


rm(list = ls())


pacman::p_load(
  dplyr,
  ggplot2,
  geobr,
  sf,
  stringr,
  mapview
)


options(encoding = "utf-8")


setwd(dirname(getActiveDocumentContext()$path))


load("educacao_amazonia_TRI2.RData")



#------------------------------------------------------------
# 1. Selecionar escolas com alternância persistente
#------------------------------------------------------------

alternancia_mapa <- educacao_estrutura_2023 %>%
  filter(alternancia_3anos == 1)


#------------------------------------------------------------
# 2. Padronizar código das escolas
#------------------------------------------------------------

alternancia_mapa <- alternancia_mapa %>%
  rename(
    code_school = CO_ENTIDADE
  ) %>%
  mutate(
    code_school = str_trim(as.character(code_school))
  )



#------------------------------------------------------------
# 3. Obter coordenadas das escolas
#------------------------------------------------------------

escolas_amazonia <- read_schools(
  year = 2023,
  showProgress = TRUE,
  cache = TRUE
)


escolas_amazonia <- escolas_amazonia %>%
  mutate(
    code_school = str_trim(as.character(code_school))
  )

#------------------------------------------------------------
# 4. Conferir correspondência entre bases
#------------------------------------------------------------

length(
  intersect(
    alternancia_mapa$code_school,
    escolas_amazonia$code_school
  )
)

#------------------------------------------------------------
# 5. Associar coordenadas às escolas
#------------------------------------------------------------

final_sf <- escolas_amazonia %>%
  inner_join(
    alternancia_mapa,
    by = "code_school"
  )



# Remover geometrias vazias

final_sf_map <- final_sf %>%
  filter(
    !sf::st_is_empty(geom)
  )

#------------------------------------------------------------
# 6. Mapa do Arco do Desmatamento
#------------------------------------------------------------


ufs_arco_desmatamento <- c(
  "PA", # Pará
  "MT", # Mato Grosso
  "TO", # Tocantins
  "RO", # Rondônia
  "MA"  # Maranhão
)


mapa_ufs <- read_state(
  year = 2020
) %>%
  filter(
    abbrev_state %in% ufs_arco_desmatamento
  )


#------------------------------------------------------------
# 7. Mapa estático para apresentação dos resultados
#------------------------------------------------------------


mapa_alternancia <- ggplot() +
  
  geom_sf(
    data = mapa_ufs,
    fill = "gray95",
    color = "gray50"
  ) +
  
  geom_sf_label(
    data = mapa_ufs,
    aes(label = abbrev_state),
    alpha = 0.2
  ) +
  
  geom_sf(
    data = final_sf_map,
    aes(color = factor(nivel_complexidade)),
    size = 2,
    alpha = 0.85
  ) +
  
  scale_color_manual(
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
    title = "Escolas com Pedagogia da Alternância no Arco do Desmatamento",
    subtitle = "Distribuição espacial das escolas rurais",
    caption = "Fonte: Censo Escolar/INEP (2023)"
  ) +
  
  theme_minimal()


mapa_alternancia

#------------------------------------------------------------
# 8. Salvar figura para o TCC
#------------------------------------------------------------


ggsave(
  "mapa_amazonia_alternancia.png",
  mapa_alternancia,
  width = 8,
  height = 7,
  dpi = 300
)

#------------------------------------------------------------
# 9. Versão para slides
#------------------------------------------------------------


mapa_slide <- mapa_alternancia +
  
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(
      size = 14,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    plot.caption = element_text(
      size = 9
    ),
    legend.text = element_text(
      size = 10
    ),
    legend.title = element_text(
      size = 11
    )
  )

mapa_slide

ggsave(
  "mapa_amazonia_alternancia_slide.png",
  mapa_slide,
  width = 8,
  height = 7,
  dpi = 300
)



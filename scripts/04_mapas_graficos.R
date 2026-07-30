rm(list=ls())
pacman::p_load(openxlsx,tidyr,dplyr,openxlsx,stats,
               data.table,PNADcIBGE,survey,ROCR,
               convey,magrittr,rstudioapi,psych,
               ggplot2,ggmosaic,rstudioapi,fastDummies,lubridate,data.table,
               geobr,mapview, mapshot,sf,stringr)
options(encoding = "utf-8")
setwd(dirname(getActiveDocumentContext()$path))

load("educacao_amazonia_TRI2.RData")

#Filtrar escolas com alternância
alternancia_mapa <- educacao_estrutura_2023 %>%
  filter(alternancia_3anos == 1)

#TROCANDO NOME DAS UNIDADES de 2023
names(alternancia_mapa)[
  names(alternancia_mapa) == "CO_ENTIDADE"
] <- "code_school"

#padronizar as bases
alternancia_mapa <- alternancia_mapa %>%
  mutate(
    code_school = stringr::str_trim(
      as.character(code_school)
    )
  )

#Padronizar código
alternancia_mapa <- alternancia_mapa %>%
  mutate(
    code_school = stringr::str_trim(
      as.character(code_school)
    )
  )


#Coordenadas das escolas
escolas_amazonia <- read_schools(
  year = 2023,
  showProgress = TRUE,
  cache = TRUE
)

escolas_amazonia <- escolas_amazonia %>%
  mutate(
    code_school = stringr::str_trim(
      as.character(code_school)
    )
  )


# Verificar quantas escolas combinam
length(
  intersect(
    alternancia_mapa$code_school,
    escolas_amazonia$code_school
  )
)


# Juntar coordenadas
final_sf <- escolas_amazonia %>%
  inner_join(
    alternancia_mapa,
    by = "code_school"
  )


# Remover geometrias vazias
final_sf_map <- final_sf %>%
  filter(!sf::st_is_empty(geom))


#Ler mapa da Amazônia Legal
ufs_amazonia <- c(
  "PA", # Pará
  "MT", # Mato Grosso
  "TO", # Tocantins
  "RO", # Rondônia
  "MA"  # Maranhão
)

mapa_ufs <- read_state(year = 2020) %>%
  filter(abbrev_state %in% ufs_amazonia)

##MAPA ESTÁTICO só mostrando a Amazônia
ggplot() +
  
  geom_sf(
    data = mapa_ufs,
    fill = "gray95",
    color = "gray50"
  ) +
  
  labs(
    title = "Escolas com Pedagogia da Alternância na Amazônia",
    subtitle = "Distribuição espacial das escolas rurais",
    caption = "Fonte: Censo Escolar/INEP"
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
      "1" = "#FFFFCC",  # amarelo bem claro
      "2" = "#FFEDA0",
      "3" = "#FED976",
      "4" = "#FEB24C",
      "5" = "#F03B20",
      "6" = "#BD0026"   # vermelho escuro
    ),
    name = "Nível de Complexidade"
  ) +
  
  theme_minimal()

#salvando
ggsave(
  "mapa_amazonia_alternancia_novo.png",
  width = 8,
  height = 7,
  dpi = 300
)



###Para o slide 
## MAPA ESTÁTICO só mostrando a Amazônia
ggplot() +
  
  geom_sf(
    data = mapa_ufs,
    fill = "gray95",
    color = "gray50"
  ) +
  
  labs(
    title = "Escolas com Pedagogia da Alternância na Amazônia",
    subtitle = "Distribuição espacial das escolas rurais",
    caption = "Fonte: Censo Escolar/INEP"
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
  
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )

#salvando para o slide
ggsave(
  "mapa_amazonia_alternancia_fonte_Arial.png",
  width = 8,
  height = 7,
  dpi = 300
)



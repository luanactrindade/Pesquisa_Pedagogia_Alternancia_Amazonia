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



##Medir o nivel de complexidade
educacao_estrutura_2023 %>%
  filter(!is.na(nivel_complexidade)) %>%
  group_by(nivel_complexidade) %>%
  summarise(
    taxa_alternancia = mean(alternancia_3anos, na.rm = TRUE)
  )

#Criar tabela
taxa_complexidade <- educacao_estrutura_2023 %>%
  filter(!is.na(nivel_complexidade)) %>%
  group_by(nivel_complexidade) %>%
  summarise(
    taxa_alternancia = mean(alternancia_3anos, na.rm = TRUE)
  )

#grafico RIDGELINE
ggplot(
  educacao_estrutura_2023 %>%
    filter(!is.na(complexidade_escola)),
  
  aes(
    x = complexidade_escola,
    y = factor(nivel_complexidade),
    fill = factor(nivel_complexidade)
  )
) +
  
  geom_density_ridges(
    alpha = 0.7
  ) +
  
  labs(
    x = "Índice de complexidade institucional",
    y = "Nível de complexidade",
    fill = "Nível"
  ) +
  
  theme_minimal()


#
ggsave(
  "densidade_alternancia.png",
  width = 6,
  height = 4,
  dpi = 150
)

#
educacao_estrutura_2023 <- educacao_estrutura_2023 %>%
  mutate(
    complexidade_percentil = percent_rank(complexidade_escola) * 100
  )

#gráfico 2 
ggplot(
  educacao_estrutura_2023 %>%
    filter(!is.na(complexidade_percentil)),
  
  aes(
    x = complexidade_percentil,
    y = factor(nivel_complexidade),
    fill = factor(nivel_complexidade)
  )
) +
  
  geom_density_ridges(
    alpha = 0.7
  ) +
  
  labs(
    x = "Percentil de complexidade institucional",
    y = "Nível de complexidade",
    fill = "Nível"
  ) +
  
  theme_minimal()

##
ggsave(
  "densidade_alternancia_porcentagem.png",
  width = 6,
  height = 4,
  dpi = 150
)

#figura 3 Distribuição da complexidade escolar
# calcular proporções por nível
dados_plot <- educacao_estrutura_2023 %>%
  filter(!is.na(nivel_complexidade)) %>%
  group_by(alternancia_3anos, nivel_complexidade) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(alternancia_3anos) %>%
  mutate(
    perc = n / sum(n)
  )

# gráfico
ggplot(
  dados_plot,
  aes(
    x = nivel_complexidade,
    y = perc,
    fill = factor(alternancia_3anos)
  )
) +
  
  geom_area(
    alpha = 0.4,
    position = "identity"
  ) +
  
  scale_x_continuous(
    breaks = 1:6
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  
  labs(
    x = "Nível de complexidade escolar",
    y = "Percentual",
    fill = "Alternância"
  ) +
  
  scale_fill_manual(
    values = c(
      "0" = "grey70",
      "1" = "#2A9D8F"
    ),
    labels = c(
      "Sem alternância",
      "Com alternância"
    )
  ) +
  
  theme_minimal()


#gráfico 3
ggplot(
  educacao_estrutura_2023 %>% 
    filter(!is.na(complexidade_escola)),
  
  aes(
    x = complexidade_escola,
    fill = factor(alternancia_3anos)
  )
) +
  
  geom_density(
    alpha = 0.4,
    color = "black"
  ) +
  
  labs(
    x = "Índice de complexidade escolar",
    y = "Densidade",
    fill = "Alternância"
  ) +
  
  scale_fill_manual(
    values = c(
      "0" = "grey85",
      "1" = "#8CCFC9"
    ),
    labels = c(
      "Sem alternância",
      "Com alternância"
    )
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
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

#quadro mostrando a estrutura das bases
#infraestrutura
infra <- educacao_estrutura_2023 %>%
  group_by(alternancia_3anos) %>%
  summarise(
    Energia    = mean(energia, na.rm = TRUE),
    Banheiro   = mean(banheiro, na.rm = TRUE),
    Biblioteca = mean(biblioteca, na.rm = TRUE),
    Cozinha    = mean(cozinha, na.rm = TRUE)
  ) %>%
  mutate(across(-alternancia_3anos, ~ round(.x * 100, 1)))

infra

#com e sem alt
infra <- infra %>%
  mutate(
    alternancia_3anos = ifelse(
      alternancia_3anos == 1,
      "Com alternância",
      "Sem alternância"
    )
  )

infra

# Água
agua <- educacao_estrutura_2023 %>%
  group_by(alternancia_3anos) %>%
  summarise(
    Rede_publica = mean(agua_rede_publica, na.rm = TRUE) * 100,
    Poco_artesiano = mean(agua_poco_artesiano, na.rm = TRUE) * 100,
    Cacimba = mean(agua_cacimba, na.rm = TRUE) * 100,
    Rio = mean(agua_rio, na.rm = TRUE) * 100
  ) %>%
  mutate(across(-alternancia_3anos, ~round(.x,1)))

agua

# Número de matrículas
matriculas <- educacao_estrutura_2023 %>%
  group_by(alternancia_3anos) %>%
  summarise(
    Media = round(mean(QT_MAT_BAS, na.rm=TRUE),1),
    Mediana = median(QT_MAT_BAS, na.rm=TRUE),
    DP = round(sd(QT_MAT_BAS, na.rm=TRUE),1)
  )

matriculas

# Maior etapa ofertada
etapa <- educacao_estrutura_2023 %>%
  mutate(
    etapa = factor(
      etapa_mais_alta,
      levels = c(1,2,3,4),
      labels = c("Infantil","Fundamental","Médio","EJA")
    )
  ) %>%
  count(alternancia_3anos, etapa) %>%
  group_by(alternancia_3anos) %>%
  mutate(
    percentual = round(100*n/sum(n),1)
  )

etapa

educacao_estrutura_2023 %>%
  count(alternancia_3anos)

#Depois refaça a distribuição do porte diretamente desse banco:
  
  educacao_estrutura_2023 %>%
  count(alternancia_3anos, porte_cat) %>%
  group_by(alternancia_3anos) %>%
  mutate(
    percentual = round(100 * n / sum(n), 1)
  )
  

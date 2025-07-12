library(arrow)
library(dplyr)
library(ggplot2)
library(scales)
library(ggridges)
library(forcats)
library(tidyr)
library(ggview)

options(scipen = 9999)

# datos ----
# cargar encuesta casen en formato parquet (más rápido de cargar)
# casen2022_2 <- arrow::read_parquet("datos/originales/casen2022.parquet")
casen2022_2 <- arrow::read_parquet("datos/casen_ingresos.parquet")
poblacion <- arrow::read_parquet("datos/censo_proyecciones_año.parquet")

color_fondo = "black"; color_texto = "white"

# obtener población comunal
poblacion_comunas <- poblacion |> 
  filter(año == 2024) |> 
  arrange(desc(población))

# procesar encuesta de diseño complejo
library(srvyr)

casen_svy <- casen2022_2 |> 
  # filtrar 80 comunas con más población
  filter(comuna %in% poblacion_comunas$comuna[1:80]) |> 
  # crear diseño de encuestas complejas
  as_survey(weights = expc, 
            strata = estrato, 
            ids = id_persona, 
            nest = TRUE)

# calcular cantidades usando factor de expansión
casen_ingresos <- casen_svy |> 
  group_by(comuna, yoprcor) |> 
  summarize(n = survey_total(),
            p = survey_mean())

# limpiar datos y limitar ingresos
casen_ingresos_2 <- casen_ingresos |> 
  rename(variable = yoprcor) |> 
  filter(!is.na(variable)) |> 
  # calcular mediana de ingresos
  group_by(comuna) |> 
  mutate(mediana = median(variable, na.rm = T)) |>
  # limitar ingresos máximos, ordenar comunas
  filter(variable < 2500000) |> 
  ungroup() |> 
  mutate(comuna = as.factor(comuna),
         comuna = fct_reorder(comuna, mediana))

# expandir observaciones
casen_ingresos_3 <- casen_ingresos_2 |> 
  mutate(n = as.integer(n)) |> 
  uncount(weights = n)

# graficar ----
number_options(decimal.mark = ",", big.mark = ".")

casen_ingresos_3 |>
  ggplot() +
  aes(x = variable, y = comuna) +
  geom_density_ridges()

casen_ingresos_3 |>
  ggplot() +
  aes(x = variable, y = comuna) +
  # densidades
  geom_density_ridges(rel_min_height = 0, scale = 4, bandwidth = 30000,
                      fill = color_fondo, color = color_texto) +
  # textos de medianas a la derecha
  geom_text(data = ~distinct(.x, variable, comuna, mediana),
            aes(label = label_currency()(mediana |> signif(digits = 2)), 
                x = 2500000), nudge_x = 120000,
            color = color_texto, 
            size = 2.5, check_overlap = T,
            hjust = 0, vjust = 0.3) +
  # expandir escala horizontal
  scale_x_continuous(expand = expansion(c(0, 0.09)), 
                     breaks = c(0, .5, 1, 1.5, 2, 2.5)*1000000,
                     labels = label_currency()) +
  # no cortar geometrías fuera del plano de coordenadas
  coord_cartesian(clip = "off") +
  # tema
  theme_void(base_family = "Helvetica") +
  # texto eje horizontal
  theme(axis.text.x = element_text(size = 6, color = color_texto, 
                                   margin = margin(t = 4))) +
  # texto eje vertical
  theme(axis.text.y = element_text(size = 7, color = color_texto, 
                                   hjust = 1, vjust = 0.3, margin = margin(r = 7))) +
  # fondo
  theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
        plot.background = element_rect(fill = color_fondo, linewidth = 0),
        plot.margin = unit(c(5, 4, 5, 4), "mm")) +
  theme(plot.title = element_text(size = 10, hjust = 0.4, colour = color_texto, margin = margin(b = 3)),
        plot.subtitle = element_text(size = 8, hjust = 0.4, colour = color_texto, margin = margin(b = 6))) +
  # textos
  labs(title = "Distribución de ingresos por comuna: Chile" |> toupper(),
       subtitle = "Ingreso de la ocupación principal, CASEN 2022") +
  canvas(8, 12)

save_ggplot(plot = last_plot(), file = "graficos/grafico_densidad_joy_division.jpg")

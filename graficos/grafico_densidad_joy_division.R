library(dplyr)
library(ggplot2)
library(ggridges)
library(forcats)
library(tidyr)

options(scipen = 999999)

#datos
casen_comunas <- arrow::read_feather("app/casen_ingresos.feather")
poblacion <- arrow::read_parquet("datos/censo_proyecciones_año.parquet")

color_fondo = "black"; color_texto = "white"

#obtener población
poblacion_comunas <- poblacion |> 
  filter(año == 2024) |> 
  arrange(desc(población))

#filtrar variable de ingresos y comunas
datos_filtrados <- casen_comunas |> 
  mutate(variable = yoprcor) |> 
  filter(!is.na(variable)) |> 
  # filter(pco1 == "1. Jefatura de Hogar") |> 
  filter(comuna %in% poblacion_comunas$comuna[1:80]) 

#limitar ingresos máximos, ordenar comunas
datos_procesados <- datos_filtrados |>
  mutate(variable = ifelse(variable >= 8000000, 8000000, variable)) |> 
  group_by(comuna) |> 
  mutate(promedio = mean(variable, na.rm = T)) |> 
  ungroup() |> 
  mutate(comuna = as.factor(comuna),
         comuna = fct_reorder(comuna, promedio))

#aplicar factor de expansión
datos_expandidos <- datos_procesados |> 
  select(comuna, expc, variable, promedio) |> 
  uncount(expc)

#graficar
grafico_exp <- datos_expandidos |>
# grafico <- datos_procesados |>
  ggplot(aes(x = variable, y = comuna)) +
  geom_density_ridges(rel_min_height = 0, alpha = 1, 
                      scale = 6, bandwidth = 47800*1.5,
                      size = 0.25,
                      fill = color_fondo, color = color_texto) +
  geom_text(aes(label = paste0(comuna, " "), x = -100000*2.5), 
            color = color_texto, nudge_y = 0,
            size = 2, check_overlap = T, 
            hjust = 1, vjust = 0.3) + 
  scale_x_continuous(expand = expansion(0), 
                     breaks = c(.5, 2, 4, 6, 8, 10)*1000000,
                     labels = ~format(.x, big.mark = ".", trim = T)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(legend.position = "none",
        axis.ticks.x = element_line(color = color_texto),
        axis.text.x = element_text(size = 6, color = color_texto, 
                                   angle = -90, vjust = 0.5, hjust = 0, 
                                   margin = margin(t=4)),
        panel.background = element_rect(fill = color_fondo, linewidth = 0),
        plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
  theme(plot.margin = unit(c(6, 10, 2, 22), "mm")
  )

grafico_exp

ggsave(grafico_exp,
       filename = "grafico_densidad_ingresos_exp.png", 
       width = 8, height = 12); beepr::beep()

ggsave(grafico,
       filename = "grafico_densidad_ingresos.png", 
       width = 8, height = 12); beepr::beep()


library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)

#cargar datos de casen2022_procesar.r
# setwd("app/")
casen2022_2 <- arrow::read_parquet("datos/casen2022.parquet")

variables_casen <- c(
  "comuna",
  "region",
  "area",
  "pco1",
  "expc",                    #factor de expansión comunal
  "sexo",                    #género
  "ytotcorh",                #Ingreso total del hogar corregido
  "ytotcor",                 #Ingreso total corregido
  "yoprcor",                 #Ingreso ocupación principal
  "ypc",                     #Ingreso total per cápita del hogar corregido
  "y2803")

# filtrar variables y aplicar factor de expansión ----
casen2022_comunas <- casen2022_2 |> 
  select(any_of(variables_casen)) |> 
  mutate(across(where(is.factor), as.character))

.variable = "ytotcorh"
.comunas = c("La Florida", "Puente Alto", "Ñuñoa", "Vitacura")

if (.variable %in% c("ytotcorh", "ypc")) {
  dato1 <- casen2022_comunas |> 
    filter(pco1 == "1. Jefatura de Hogar")
} else {
  dato1 <- casen2022_comunas
}

color_fondo = "#222222"
color_secundario = "#333333"
color_detalle = "#333333"
color_texto = "#999999"
color_destacado = "#BBBBBB"

dato2 <- dato1 |> 
  # filter(comuna %in% .comunas) |> 
  mutate(comuna_seleccionada = ifelse(comuna %in% .comunas, TRUE, FALSE)) |> 
  mutate(variable = !!sym(.variable)) |>
  group_by(comuna, comuna_seleccionada) |> 
  summarize(variable = mean(variable, na.rm = TRUE), .groups = "drop")

dato2 |> 
  ggplot(aes(x = variable, y = 1,
             # fill = comuna_seleccionada, color = comuna_seleccionada, 
             size = comuna_seleccionada, alpha = comuna_seleccionada)) +
  geom_vline(xintercept = mean(dato2$variable), 
             linewidth = 1.6, linetype = "dashed", color = color_secundario) +
  geom_jitter(data = dato2 |> filter(!comuna_seleccionada), 
              color = color_texto, width = 0, height = 0.3) +
  geom_jitter(data = dato2 |> filter(comuna_seleccionada), 
              aes(color = comuna),
              width = 0, height = 0.3) +
  # geom_point() +
  scale_size_manual(values = c(5, 10), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 0.7, "FALSE" = 0.3), guide = "none") +
  scale_y_continuous(limits = c(0, 2)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(label = ~format(.x, big.mark = ".", decimal.mark = ",")) +
  #fondo
  theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
        plot.background = element_rect(fill = color_fondo, linewidth = 0),
        legend.background = element_rect(fill = color_fondo, linewidth = 0),
        legend.key = element_rect(fill = color_fondo)) +
  #otros
  theme(axis.line.x = element_line(linewidth = 2, color = color_detalle, lineend = "round"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.5, color = color_detalle),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(color = color_texto, size = 13),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 5)),
        legend.text = element_text(color = color_texto, size = 13, margin = margin(t= 4, b = 4, r = 14)),
        # legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(1, 10, 1, 1), "mm")
  ) +
  theme(legend.position = "none")
  # guides(
  #   # colour = guide_colourbar(order = 1),
  #   # shape = guide_legend(order = 2),
  #   size = "none"
  # )

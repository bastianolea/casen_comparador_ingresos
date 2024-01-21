#en este script se cargan los datos de la casen preprocesados en casen2022_procesar.r, 
#y se seleccionan las variables a utilizar en el visualizador, calculando las medidas 
#necesarias para optimizar el rendimiento en vivo de la app.

library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)

#cargar datos de casen2022_procesar.r
casen2022_2 <- arrow::read_parquet("datos/casen2022.parquet")

variables_casen <- c(
  "comuna",
  "region",
  # "area",
  "pco1",
  "expc",                    #factor de expansión comunal
  # "sexo",                    #género
  "ytotcorh",                #Ingreso total del hogar corregido
  "ytotcor",                 #Ingreso total corregido
  "yoprcor",                 #Ingreso ocupación principal
  "ypc",                     #Ingreso total per cápita del hogar corregido
  "y2803")

# filtrar variables y aplicar factor de expansión ----
casen2022_comunas <- casen2022_2 |> 
  select(any_of(variables_casen)) |> 
  mutate(across(where(is.factor), as.character))

# arrow::write_feather(casen2022_comunas, "app/casen_ingresos.feather")

secuencia_salto = 500000
secuencia_maximo = 5000000
secuencia_ingresos <- seq(0, secuencia_maximo, by = secuencia_salto)

secuencia_ingresos_rango_etiqueta <- tibble(secuencia_ingresos) |> 
  mutate(miles = paste(secuencia_ingresos/1000),
         millones = paste(secuencia_ingresos/1000000)) |> 
  mutate(secuencia_ingresos_etiqueta = case_when(secuencia_ingresos < secuencia_salto ~ paste("Menos de", secuencia_salto/1000, "mil"),
                                                 secuencia_ingresos < 1000000 & lead(secuencia_ingresos) == 1000000 ~ paste(miles, "a", lead(millones), "millón"),
                                                 secuencia_ingresos < 1000000 ~ paste(miles, "mil a", lead(miles), "mil"),
                                                 secuencia_ingresos == 1000000 ~ paste(millones, "a", lead(millones), "millones"),
                                                 secuencia_ingresos < secuencia_maximo ~ paste( millones, "a", lead(millones), "millones"),
                                                 secuencia_ingresos == secuencia_maximo ~ paste(secuencia_maximo/1000000, "millones o más")
  )) |> 
  pull(secuencia_ingresos_etiqueta)

secuencia_ingresos_etiqueta <- tibble(secuencia_ingresos) |> 
  mutate(miles = paste(secuencia_ingresos/1000),
         millones = paste(secuencia_ingresos/1000000)) |> 
  mutate(secuencia_ingresos_etiqueta = case_when(#secuencia_ingresos < secuencia_salto ~ paste("Menos de", secuencia_salto/1000, "mil"),
    secuencia_ingresos < secuencia_salto ~ paste("0"),
    # secuencia_ingresos < 1000000 & lead(secuencia_ingresos) == 1000000 ~ paste(miles, "a", lead(millones), "millón"),
    secuencia_ingresos < 1000000 ~ paste(miles, "mil"),
    secuencia_ingresos == 1000000 ~ paste(millones, "millón"),
    secuencia_ingresos == secuencia_maximo ~ paste(secuencia_maximo/1000000, "millones o más"),
    secuencia_ingresos > 1000000 ~ paste(millones, "millones")
  )) |> 
  pull(secuencia_ingresos_etiqueta)

.variable = "ytotcorh"
.comunas = c("La Florida", "Puente Alto", "Ñuñoa", "Vitacura")
.limite_x = secuencia_maximo
.detalle = 2
.variable_color = "comuna"
# .variable_color = "sexo"

if (.variable %in% c("ytotcorh", "ypc")) {
dato1 <- casen2022_comunas |> 
  filter(pco1 == "1. Jefatura de Hogar")
} else {
  dato1 <- casen2022_comunas
}
  
dato2 <- dato1 |> 
  filter(comuna %in% .comunas) |> 
  mutate(variable = !!sym(.variable)) |> 
  mutate(variable = ifelse(variable > .limite_x, .limite_x, variable))

dato3 <- dato2 |> 
  tidyr::uncount(weights = expc) #factor de expansión

dato3 |> 
  ggplot(aes(fill = .data[[.variable_color]])) +
  geom_density(aes(variable), alpha = 0.5, linewidth = 0, adjust = .detalle) +
  # geom_vline(aes(xintercept = 500000)) +
  scale_x_continuous(breaks = secuencia_ingresos,
                     labels = secuencia_ingresos_etiqueta,
                     expand = expansion(0)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  #fondo
  theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
        plot.background = element_rect(fill = color_fondo, linewidth = 0),
        legend.background = element_rect(fill = color_fondo, linewidth = 0),
        legend.key = element_rect(fill = color_fondo)) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  theme(axis.line.x = element_line(linewidth = 2, color = color_texto, lineend = "round"),
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
        axis.title = element_blank()
  )



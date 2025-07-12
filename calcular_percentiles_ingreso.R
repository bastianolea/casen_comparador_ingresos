#script para calcular % de población comunal o nacional que gana más o menos que x

# install.packages("spatstat")

library(dplyr)
library(stringr)
library(glue)
library(readr)
library(spatstat) #weighted.median y weighted.quartile
library(purrr)
library(ggplot2)

options(scipen = 9999)

#cargar datos
casen <- arrow::read_parquet("datos/casen2022.parquet")
poblacion <- arrow::read_parquet("datos/censo_proyecciones_año.parquet")

#filtrar datos
casen_ingresos <- casen |> 
  select(expc, expr, comuna, region, ytrabajocor) |> 
  filter(!is.na(ytrabajocor))

#función que permite encontrar a qué numero se parece más otro número, 
#lo que sirve para obtener el decil o percentil que venga en un vector de cuantiles, ya que te dice en cual de esos numeros se ubica
#o sea que si le das las 10 cifras que cortan los deciles, y le das un ingreso, te retorna el decil
encontrar_decil <- function(casen_porcentajes_ingreso = NULL, ingreso = 500000) {
  which(abs(casen_porcentajes_ingreso - ingreso) == min(abs(casen_porcentajes_ingreso - ingreso)))
}

# calculo comunal ----
casen_percentiles_comuna <- map(unique(casen_ingresos$comuna), ~{ 
  casen_ingresos_comuna <- casen_ingresos |> filter(comuna == .x) #filtrar datos por la comuna
  
  #obtener cortes de los percentiles: vector de 100 numeros que cortan la distribución de ingresos
  casen_porcentajes_ingreso <- weighted.quantile(casen_ingresos_comuna$ytrabajocor, 
                                                 w = casen_ingresos_comuna$expr, 
                                                 probs = seq(.01, .99, by = .01)) |>
    as.integer()
  
  #encontrar a qué percentil pertenece cada ingreso, y luego agrupar los percentiles y ver cuales son los cortes (podría ser mucho más simple pero la idea es obtener más datos)
  percentiles_comuna <- casen_ingresos_comuna |> 
    rowwise() |> 
    mutate(percentil = encontrar_decil(casen_porcentajes_ingreso, ytrabajocor)[1]) |> 
    group_by(percentil) |> 
    summarize(promedio_ingresos = weighted.mean(ytrabajocor, expc) |> round(0),
      mediana_ingresos = weighted.median(ytrabajocor, expc) |> round(0),
      decil_min_ingresos = min(ytrabajocor),
      decil_max_ingresos = max(ytrabajocor)
    ) |> 
    mutate(comuna = .x) |> 
    relocate(comuna, .before = percentil)
  return(percentiles_comuna)
}, .progress = TRUE)

# #porcentaje de la población que gana más de x
# casen_percentiles_comuna |> 
#   list_rbind() |> 
#   filter(mediana_ingresos >= 3500000) |> #x
#   group_by(comuna) |> 
#   filter(percentil == min(percentil)) |> 
#   ungroup() |> 
#   select(comuna, percentil) |> 
#   mutate(porcentaje = 100-percentil)

#comunas de la rm
comunas_rm <- casen_ingresos |> 
  select(comuna, region) |> 
  filter(region == "Región Metropolitana de Santiago") |> 
  distinct(comuna) |> 
  pull()

#comunas de la rm, con percentiles de ingresos en los que se gana más de 3500000
comunas_rm_mayores <- casen_percentiles_comuna |> 
  list_rbind() |> 
  filter(comuna %in% comunas_rm) |> 
  filter(decil_min_ingresos >= 3200000) |> 
  group_by(comuna) |> 
  slice_min(percentil) |> #percentil de corte
  ungroup() |> 
  mutate(porcentaje = 100-percentil) |> #porcentaje de la población
  arrange(percentil)

#comunas donde no hay registro de ingresos mayoers a 3500000
comunas_rm_restantes <- comunas_rm[!(comunas_rm %in% comunas_rm_mayores$comuna)]

comunas_rm_menores <- tibble("comuna" = comunas_rm_restantes) |> 
  mutate(percentil = 100,
         porcentaje = 0)

#unir comunas mayores y menores, adjuntar población, filtrar comunas con 1% de población de la región
comunas_rm_todas <- bind_rows(comunas_rm_mayores, comunas_rm_menores) |> 
  left_join(poblacion |> filter(año == 2024) |> select(comuna, población), by = "comuna") |> 
  mutate(poblacion_p = población/sum(población)) |> 
  filter(poblacion_p > 0.01)

## gráfico ----
comunas_rm_todas |> 
  mutate(comuna = forcats::fct_reorder(comuna, porcentaje)) |> 
  mutate(etiqueta = if_else(porcentaje == 0, "<1%", paste0(porcentaje, "%"))) |> 
  ggplot(aes(y = comuna)) +
  geom_col(aes(x = 100), fill = "#f65b74" , alpha= .6) +
  geom_col(aes(x = porcentaje), fill = "#23b0bd") +
  geom_text(aes(label = etiqueta, x = porcentaje+2),
            hjust = 0, size = 3.4) +
  scale_x_continuous(labels = ~paste0(.x, "%"), expand = expansion(c(0, 0.03))) +
  theme_minimal() +
  labs(title = "Porcentaje de la población que recibe ingresos mensuales \nsuperiores a 3,2 millones de pesos",
         subtitle = "Comunas de la Región Metropolitana",
       caption = "Fuente: Encuesta Casen 2022", 
       y = NULL, x = "Porcentaje de la población") +
  theme(plot.title.position = "plot", 
        panel.grid.major.y = element_blank(), 
        axis.text.y = element_text(color = "black", size = 10))

ggsave("graficos/grafico_porcentaje_ingresos.png", scale = 1.8)

# calculo nacional ----

casen_porcentajes_ingreso_nacional <- weighted.quantile(casen_ingresos$ytrabajocor, w = casen_ingresos$expr, probs = seq(.01, .99, by = .01)) |>
  as.integer() |>
  signif(5)

casen_ingresos |> 
  rowwise() |> 
  mutate(percentil = encontrar_decil(casen_porcentajes_ingreso_nacional, ytrabajocor)[1]) |> 
  group_by(percentil) |> 
  summarize(
    promedio_ingresos = weighted.mean(ytrabajocor, expc) |> round(0),
    mediana_ingresos = weighted.median(ytrabajocor, expc) |> round(0),
    decil_min_ingresos = min(ytrabajocor),
    decil_max_ingresos = max(ytrabajocor)
  ) |> 
  filter(decil_min_ingresos >= 3200000) |> 
  mutate(porcentaje = 100-percentil)

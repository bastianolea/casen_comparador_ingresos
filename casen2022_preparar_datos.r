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

arrow::write_feather(casen2022_comunas, "app/casen_ingresos.feather")
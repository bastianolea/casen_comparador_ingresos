
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)

#cargar datos de casen2022_procesar.r
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

dato2 <- dato1 |> 
  filter(comuna %in% .comunas) |> 
  mutate(variable = !!sym(.variable)) |> 
  mutate(variable = ifelse(variable > .limite_x, .limite_x, variable))

dato3 <- dato2 |> 
  tidyr::uncount(weights = expc) #factor de expansión


#prueba de poner los pesos en el gráfico y no multiplicando filas
set.seed(42)
df <- data.frame(x = rnorm(100), y = abs(rnorm(100)), type = c(1, 5))

p <- ggplot(df, aes(x, colour = factor(type)))

# Weights are pointless here because they are by group
p + geom_density(aes(weight = type, y = after_stat(count)))

#comparación de resultados de gráficos con distintas formas de peso
dato2 |>
  ggplot(aes(ytotcorh, colour = comuna)) +
  geom_density() +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "sin pesos")
dato2 |> 
  ggplot(aes(ytotcorh, colour = comuna)) +
  geom_density(aes(weight = expc, y = after_stat(density))) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "con pesos")
dato3 |> 
  ggplot(aes(color = comuna)) +
  geom_density(aes(ytotcorh), adjust = 3) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "con expc")
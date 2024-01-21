#en este script se carga la base de datos descargada en el paso casen2022_importar.r y se deja guardada en un formato más eficiente

library(dplyr)

#preprocesar ----
message("cargando casen 2022...")

#obtener codigos unicos territoriales desde libro de codigos
cut_comunas <- readxl::read_excel("manuales/Libro_de_codigos_Base_de_datos_provincia_y_comuna_Casen_2022.xlsx") |> 
  janitor::row_to_names(2) |> 
  janitor::clean_names() |> 
  tidyr::fill(nombre_variable) |> 
  filter(nombre_variable == "comuna") |> 
  select(cut_comuna = valores, comuna = etiquetas_de_valores) |> 
  filter(!is.na(cut_comuna), !is.na(comuna))

#cargar columans de comunas
casen2022comunas <- readstata13::read.dta13("datos/Base de datos provincia y comuna Casen 2022 STATA.dta", generate.factors = T) |> 
  as_tibble() |> 
  left_join(cut_comunas, join_by(comuna))

#cargar base
casen2022 <- readstata13::read.dta13("datos/Base de datos Casen 2022 STATA.dta" , generate.factors = T) |> 
  as_tibble()

#unir base con comunas
casen2022_2 <- casen2022 |> 
  left_join(casen2022comunas, join_by(folio, id_persona)) |> 
  select(names(casen2022comunas), everything()) |> 
  select(-id_vivienda, -folio, -id_persona, -cod_upm, -estrato, -varstrat, -varunit, -fecha_entrev)

#guardar
arrow::write_parquet(casen2022_2, "datos/casen2022.parquet")

# # eliminar base de datos
# file.remove("datos/Base de datos Casen 2022 STATA.dta")

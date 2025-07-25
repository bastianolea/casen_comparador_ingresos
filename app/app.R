library(shiny)
library(dplyr)
library(ggplot2)
library(ggridges)
library(shinyWidgets)
library(stringr)
library(fresh)
library(shinycssloaders)
library(glue)
library(spatstat)
library(gt)

options(scipen=999)

# color_fondo = "#222222"
# color_secundario = "#333333"
# color_detalle = "#333333"
# color_texto = "#999999"
# color_destacado = "#BBBBBB"


# color_fondo = "#444034" #"#44443a
color_fondo = "#2a241d"
color_texto = "#cdcdb7"
color_secundario = "#9a906e"
color_detalle = "#444034"
color_destacado = "#cdb876"

# color_fondo = "#242017"
# color_texto = "#DAD1BD"
color_secundario = "#B5A785"
color_detalle = "#47402E"
# color_destacado = "#cdb876"

# diccionario de variables
# source("variables.R")

#cargar datos ----
# setwd("app/")
casen_comunas <- arrow::read_parquet("casen_ingresos.parquet")
variables_numericas <- casen_comunas |> select(where(is.numeric)) |> names()

#—----
# ui ----
ui <- fluidPage(
  title = "Comparador de ingresos Casen", 
  lang = "es",
  
  use_googlefont("Aleo"), #cargar fuente o tipo de letra
  
  # use_googlefont("Urbanist"),
  
  use_theme(create_theme(
    theme = "default",
    bs_vars_input(bg = color_fondo),
    bs_vars_global(body_bg = color_fondo, 
                   text_color = color_texto, 
                   link_color = color_secundario),
    bs_vars_font(size_base = "16px", #aumentar globalmente tamaño de letra  
                 # family_sans_serif = "Urbanist" #cargar fuente o tipo de letra
    ), 
    bs_vars_modal(content_bg = color_fondo, content_border_color = color_detalle, 
                  backdrop_bg = color_fondo, backdrop_opacity = "60%"),
    bs_vars_button(
      default_color = color_fondo,
      default_bg = color_secundario,
      default_border = color_fondo, 
      border_radius_base = "6px"
    )
  )),
  
  # css ----
  tags$style(paste0(
    "h1 {
    font-size: 250%;
    font-weight: bold;
    color: ", color_destacado, ";
    }")),
  
  tags$style(paste0("
    h1, h2, h3 {
    font-weight: bold;
    font-family: Aleo;
    }")),
  
  tags$style(paste0("
    strong {
    font-weight: bold;
    color: ", color_secundario, ";
    }")),
  
  #labels de inputs
  tags$style(paste0("
    h4 {
    font-style: italic;
    font-size: 100%;
    margin-bottom: -16px;
    margin-top: 0px;
    font-family: Aleo;
    }")),
  
  #enlaces
  tags$style(paste0("
    a {
    text-decoration: underline;
    color: ", color_secundario, ";
    }
    
    a:hover {
    color: ", color_destacado, ";
    }")),
  
  #texto de pickers
  tags$style(paste0("
                    .btn.dropdown-toggle {
                   font-size: 85%;
                   }")),
  
  #colores pickers
  tags$style(paste0("
         .dropdown-menu,  .divider {
          color: black !important;
         background: ", color_detalle, " !important;
         }
  
         .dropdown-header {
         color: black !important;
         font-family: Aleo;
         font-weight: bold;
         font-size: 110%;
         }
         
         .text {
         font-size: 80%;
         color: white;
         }
         
         .form-control {
         color: ", color_texto, " !important;
         box-shadow: none;
         }
         
         .no-results {
         color: black !important;
         background: ", color_detalle, " !important;
         }
         
         .selected {
         background-color: ", color_secundario, " !important;
         color: ", color_detalle, " !important;
         }
         
         .bs-placeholder, .bs-placeholder:active, bs-placeholder:focus, .bs-placeholder:hover {
         color: ", color_fondo, " !important;
         }
         
         /*color de fondo de opción elegida*/
         .dropdown-item.selected {
         background-color: ", color_fondo, " !important;
         color: black !important;
         }
         
         /*color del fondo de la opción en hover*/
         .dropdown-item:hover {
         color: red;
         background-color: ", color_secundario, " !important;
         }
         ")),
  
  #botones, botones hover
  tags$style(paste0("
    .action-button {
    opacity: 0.6; font-size: 80%; padding: 4px; padding-left: 8px; padding-right: 8px; color: black; 
    border: 3px solid", color_detalle, ";
    }
    .action-button:hover, .action-button:active, .action-button:focus {
    opacity: 1;
    color: black; 
    border: 3px solid", color_detalle, ";
    }")),
  
  #botones radio button
  tags$style(paste0("
    .radio-group-buttons, .btn-group {
    margin-top: 0px;
    }
    
    .radiobtn, .radiobtn.active {
    font-size: 90%;
    }
    
    .radiobtn.active {
    font-weight: bold;
    }")),
  
  
  #estilo de barra slider
  tags$style("
  /*fondo de barra, o sección inactiva*/
  .irs--shiny .irs-line {
  background: none;
  background-color: ", color_detalle, ";
  }
  
  /*sección izquierda de barra activa*/
  .irs--shiny .irs-bar {
  background-color: ", color_secundario, ";
  border: 3px solid ", color_detalle, ";
  }
   /*pelota de slider*/
  .irs--shiny .irs-handle {
  background-color: ", color_detalle, ";
  box-shadow: none;
  border: 3px solid ", color_detalle, ";
  height: 30px; width: 30px;
  }
  
  /*pelota de slider hover*/
  .irs--shiny .irs-handle:hover, .irs--shiny .irs-handle:active {
  background-color: ", color_destacado, ";
  }
  
  
  .irs--shiny .irs-single {
  background-color: ", color_detalle, ";
  }
  
  .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-single {
  font-size: 70%;
  margin-top: -4px;
  }
  
  /*grosor de barra*/
  .irs--shiny .irs-line, .irs--shiny .irs-bar {
  top: 28px;
  height: 8px;
  border: 0;
  }
  
  /*etiqueta de barra*/
  .control-label {
  margin-bottom: 28px;
  }
  "),
  
  #separador
  tags$style(paste0("
                    hr {
  border-top: 3px solid ", color_detalle, ";
                    }")),
  
  
  # header ----
  fluidRow(  
    column(12,
           div(style = "margin-bottom: 16px;",
               h1("Comparador de ingresos Casen 2022"),
               em("Bastián Olea Herrera")
           ),
           p("Con este visualizador puedes generar gráficos de densidad que represente los ingresos de la población de cualquier comuna del país, o de varias comunas a la vez, para poder comparar las realidades económicas de sus habitantes."), 
           
           p("Un", strong("gráfico de densidad"), "indica cómo se distribuye una población con respecto a una variable indicada en el eje horizontal. 
           En este caso, el eje horizontal corresponde a una", strong("escala de ingresos,"), "y el eje vertical es la", strong("proporción de la población.")),
           p("Los datos de este visualizador provienen de la",
             tags$a("Encuesta de caracterización socioeconómica nacional (Casen) 2022.", target = "_blank", href = "https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2022"))
           
    ),
    column(12,
           hr()
    )
  ),
  
  
  # selectores ----
  
  fluidRow(
    column(4,
           fluidRow(
             column(12,
                    
                    h3("Seleccionar variables")
             )
           ),
           fluidRow(
             column(12,
                    p("Seleccione la variable de ingresos que desee graficar:"),
                    column(12, align = "center",
                           div(style = "max-width: 350px;",
                               radioGroupButtons(
                                 inputId = "variable",
                                 label = NULL,
                                 width = "100%",  justified = T,
                                 # choices = c("Ingresos individuales", "Ingresos de los hogares")
                                 direction = "vertical",
                                 choices = c("Ingresos individuales" = "ytotcor",
                                             "Ingresos de la ocupación principal" = "yoprcor",
                                             "Ingresos de los hogares" = "ytotcorh",
                                             "Ingreso per cápita de hogares" = "ypc",
                                             "Jubilación o pensión por vejez" = "y2803")
                               )
                           )
                    )
             ),
             
             column(12, #style = "margin-top: 18px;",
                    hr(),
                    p("Elija una o más regiones para luego elegir una o varias comunas que serán incluidas en el gráfico."),
                    pickerInput("regiones",
                                label = h4("Regiones"),
                                width = "100%",
                                multiple = TRUE,
                                # selected = "Región Metropolitana de Santiago", 
                                choices = NULL,
                                # choices = c("Todas las regiones", as.character(unique(casen_comunas$region))),
                                options = list(width = FALSE,
                                               noneSelectedText = "Sin selección")
                    )
             ),
             column(12,  
                    div(   
                      pickerInput("comunas",
                                  label = h4("Comunas"),
                                  width = "100%",
                                  multiple = TRUE,
                                  choices = NULL,
                                  options = list(maxOptions = 5, 
                                                 maxOptionsText = "Máximo 5",
                                                 noneSelectedText = "Sin selección",
                                                 width = FALSE)
                      ),
                      actionButton("azar_comunas", "Elegir comunas al azar")
                    ),
                    # )
                    hr()
                    
             ),
             
             # opciones del gráfico ----
             column(12, #align = "center", 
                    div(style = "max-width: 400px;",
                        
                        div(
                          style = "margin-top: 18px;",
                          sliderTextInput(
                            inputId = "detalle",
                            label = h4("Nivel de detalle de la curva"), width = "100%",
                            choices = c("Bajo", "Normal", "Alto"), 
                            selected = c("Normal")
                          )
                        ),
                        div(
                          style = "margin-top: 24px;",
                          sliderInput(
                            inputId = "maximo",
                            label = h4("Límite máximo de ingresos"), width = "100%",
                            min = 1000000, max = 10000000, step = 2000000, sep = ".",
                            ticks = FALSE, value = 5000000
                          )
                        )
                    ),
                    hr()
             )
             
           ),
           
           
           
    ),
    
    #graficos ----
    column(8,
           fluidRow(
             # column(12,
             #        # hr(),
             #        # h3("Visualizar"),
             #        
             # ),
             # column(12, h4("Gráfico de densidad de ingresos", style = "margin-bottom: 2px;")),
             column(12, align = "center", style = "padding: 10px; padding-bottom: 0;",
                    plotOutput("grafico_densidad", width = "100%", height = 600) |> 
                      withSpinner(color = color_destacado, type = 8)
             ),
             
             # column(12, h4("Gráfico de dispersión de ingresos", style = "margin-bottom: 2px;")),
             column(12, align = "center", style = "padding: 10px; padding-top: 0;",
                    plotOutput("grafico_dispersion", width = "100%", height = 180) |> 
                      withSpinner(color = color_destacado, type = 8)
             ),
             
             # column(12, h4("Gráfico de densidad de ingresos", style = "margin-bottom: 2px;")),
             column(12, align = "center", style = "padding: 10px; padding-bottom: 0;",
                    plotOutput("grafico_densidad_2", width = "100%", height = 600) |> 
                      withSpinner(color = color_destacado, type = 8)
             ),
             
             
             ## tablas ----
             column(12, style = "padding: 10px; padding-bottom: 0;",
                    hr(),
                    h3("Tabla de ingresos por comuna"),
                    br(),
                    
                    h4("Ingresos medianos", style = "margin-bottom: 2px;"),
                    gt_output("tabla_ingresos_medianos") |> 
                      withSpinner(color = color_destacado, type = 8),
                    
                    br(),
                    h4("Ingresos promedio", style = "margin-bottom: 2px;"),
                    gt_output("tabla_ingresos_promedio") |> 
                      withSpinner(color = color_destacado, type = 8)
             ),
             
             ### explicación ----
             column(12, style = "padding-top: 16px;",
                    hr(),
                    h3("Explicación"),
                    
                    p("La primera visualización es un", strong("gráfico de densidad"), "que que representa a toda la población de la comuna, donde la altura de la curva equivale a una mayor proporción de las personas que perciben los ingresos que indica el eje horizontal.
           Por ejemplo, un gráfico con mucha altura en la parte inferior de la escala (izquierda) significa que la mayoría de la población percibe ingresos bajos."),
           p("La segunda visualización ubica las comunas seleccionadas horizontalmente según los", strong("ingresos promedio"), "de sus habitantes (que se corresponde con el punto más denso de la visualización de arriba). En el fondo pueden verse todas las demás comunas del país (en gris), para tener un contexto cómo se comparan las comunas seleccionadas con respecto a los ingresos promedio de las demás comunas del país."), 
           p("La tercera visualización es otro gráfico de densidad, solamente que expresado de una forma distinta: con una comuna debajo de la otra, en vez de cada una superpuesta encima de las otras."),
             ),
           
           )
           
           # # opciones del gráfico
           # fluidRow(
           #   column(12,
           #          hr()
           #   ),
           #   column(12, align = "center", 
           #          div(
           #            
           #            style = "max-width: 400px;",
           #            div(
           #              style = "margin-top: 18px;",
           #              sliderTextInput(
           #                inputId = "detalle",
           #                label = h4("Nivel de detalle de la curva"), width = "100%",
           #                choices = c("Bajo", "Normal", "Alto"), 
           #                selected = c("Normal")
           #              )
           #            ),
           #            div(
           #              style = "margin-top: 24px;",
           #              sliderInput(
           #                inputId = "maximo",
           #                label = h4("Límite máximo de ingresos"), width = "100%",
           #                min = 1000000, max = 10000000, step = 2000000, sep = ".",
           #                ticks = FALSE, value = 5000000
           #              )
           #            )
           #          )
           #   )
           # )
    ),
    
    # firma ----
    fluidRow(
      column(12, style = "padding: 28px;",
             hr(),
             
             markdown("Desarrollado y programado por [Bastián Olea Herrera.](https://bastian.olea.biz)"),
             
             markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
             
             p(
               "Código de fuente de esta app y del procesamiento de los datos",
               tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/casen_comparador_ingresos")
             ),
             div(style = "height: 40px")
             
      )
    )
  )
)


#—----

server <- function(input, output, session) {
  
  #selectores ----
  updatePickerInput(session, "regiones",
                    choices = c("Todas las regiones", as.character(unique(casen_comunas$region))),
                    selected = "Región Metropolitana de Santiago",
                    options = list(width = FALSE, noneSelectedText = "Sin selección")
  )
  
  #filtrar comunas según regiones elegidas
  lista_comunas <- reactive({
    if ("Todas las regiones" %in% input$regiones) {
      lista_comunas <- split(unique(casen_comunas$comuna) |> sort(), 
                             unique(casen_comunas$region))
    } else {
      # browser()
      casen_region <- casen_comunas |> 
        filter(region %in% input$regiones) |> 
        select(region, comuna) |> 
        arrange(region, comuna) |> 
        distinct()
      
      lista_comunas <- split(casen_region, ~region) |> lapply(pull, comuna)
    }  
    return(lista_comunas)
  }) |>
    bindEvent(input$regiones)
  
  
  #pone las comunas en el selector de comunas según la región elegida
  observeEvent(input$regiones, {
    req(length(input$regiones) > 0)
    
    updatePickerInput(session,
                      inputId = "comunas",
                      choices = lista_comunas(),
                      selected = c("La Pintana", "Ñuñoa", "Vitacura"),
                      options = list(maxOptions = 5, 
                                     maxOptionsText = "Máximo 5",
                                     width = FALSE,
                                     noneSelectedText = "Sin selección")
    )
  })
  
  #comunas al azar
  observeEvent(input$azar_comunas, {
    azar_comunas <- sample(unlist(lista_comunas()), 4)
    updatePickerInput(session,
                      inputId = "comunas",
                      selected = azar_comunas,
                      options = list(maxOptions = 5, 
                                     maxOptionsText = "Máximo 5",
                                     width = FALSE,
                                     noneSelectedText = "Sin selección")
    )
  })
  
  
  # datos ----
  
  datos <- reactive({
    req(length(input$comunas) > 0)
    
    #filtrar por hogares si la variable lo requiere
    if (input$variable %in% c("ytotcorh", "ypc")) {
      dato1 <- casen_comunas |> 
        filter(pco1 == "1. Jefatura de Hogar")
    } else {
      dato1 <- casen_comunas
    }
    
    dato2 <- dato1 |> 
      #filtrar comunas
      filter(comuna %in% input$comunas) |> 
      #crear variable con los datos elegidos
      mutate(variable = !!sym(input$variable))
    
    return(dato2)
  })
  
  
  datos_densidad <- reactive({
    req(length(input$comunas) > 0)
    req(datos())
    
    dato2 <- datos() |> 
      #filtrar comunas
      filter(comuna %in% input$comunas) |> 
      #crear variable con los datos elegidos
      mutate(variable = !!sym(input$variable)) |> 
      #limitar máximo
      mutate(variable = ifelse(variable >= secuencia_maximo(), 
                               secuencia_maximo(), 
                               variable))
    
    return(dato2)
  })
  
  
  datos_dispersion <- reactive({
    req(length(input$comunas) > 0)
    # req(datos())
    # browser()
    
    #filtrar por hogares si la variable lo requiere
    if (input$variable %in% c("ytotcorh", "ypc")) {
      dato1 <- casen_comunas |> 
        filter(pco1 == "1. Jefatura de Hogar")
    } else {
      dato1 <- casen_comunas
    }
    
    dato2 <- dato1 |> 
      #crear variable con los datos elegidos
      mutate(variable = !!sym(input$variable)) |> 
      select(comuna, expc, variable)
    
    dato3 <- dato2 |> 
      # filter(comuna %in% .comunas) |> 
      mutate(comuna_seleccionada = ifelse(comuna %in% input$comunas, TRUE, FALSE)) |>
      # mutate(variable = !!sym(.variable)) |>
      group_by(comuna, comuna_seleccionada) |> 
      mutate(across(where(is.numeric), as.double)) |> 
      # summarize(variable = mean(variable, na.rm = TRUE), .groups = "drop")
      summarize(variable = weighted.mean(variable, w = expc, na.rm = TRUE), .groups = "drop") #calcular mediana con pesos
    
    # browser()
    return(dato3)
  })
  
  
  #máximo de ingresos ----
  observeEvent(input$variable, {
    maximo_por_variable = switch(input$variable,
                                 "y2803" = 2000000, #pensiones
                                 "ytotcorh" = 8000000, #hogares
                                 "ytotcor" = 3000000, #personas
                                 "yoprcor" = 3000000, #personas
                                 "ypc" = 8000000 #per capita
    )
    updateSliderInput(session, "maximo", 
                      value = maximo_por_variable
    )
  })
  
  
  # secuencia de ingresos ----
  secuencia_salto <- reactive({
    if (input$variable %in% c("y2803", "ytotcor")) {
      200000
    } else {
      500000
    }
  }) #cantidad por la que salta la secuencia, también el mínimo
  secuencia_maximo <- reactive({input$maximo}) #máximo de la secuencia
  
  #crear secuencia de ingresos (da los breaks al eje x del gráfico)
  seq_ingresos <- reactive({
    seq(0, 
        secuencia_maximo(), 
        by = secuencia_salto())
  })
  
  #etiqueta redactada de la secuencia de rangos (hace que 500.000 sea "desde 500 mil", etc.)
  secuencia_ingresos_rango_etiqueta <- reactive({
    secuencia_ingresos_rango_etiqueta <- tibble(seq_ingresos()) |> 
      rename(secuencia_ingresos = 1) |> 
      mutate(miles = paste(secuencia_ingresos/1000),
             millones = paste(secuencia_ingresos/1000000)) |> 
      mutate(secuencia_ingresos_etiqueta = case_when(secuencia_ingresos < secuencia_salto() ~ paste("Menos de", secuencia_salto()/1000, "mil"),
                                                     secuencia_ingresos < 1000000 & lead(secuencia_ingresos) == 1000000 ~ paste(miles, "a", lead(millones), "millón"),
                                                     secuencia_ingresos < 1000000 ~ paste(miles, "mil a", lead(miles), "mil"),
                                                     secuencia_ingresos == 1000000 ~ paste(millones, "a", lead(millones), "millones"),
                                                     secuencia_ingresos < secuencia_maximo() ~ paste( millones, "a", lead(millones), "millones"),
                                                     secuencia_ingresos == secuencia_maximo() ~ paste(secuencia_maximo()/1000000, "millones o más")
      )) |> 
      pull(secuencia_ingresos_etiqueta)
    
    return(secuencia_ingresos_rango_etiqueta)
  })
  
  #etiqueta redactada de la secuencia (hace que 500.000 sea "500 mil", etc.)
  secuencia_ingresos_etiqueta <- reactive({
    tibble(seq_ingresos()) |> 
      rename(secuencia_ingresos = 1) |> 
      mutate(miles = paste(secuencia_ingresos/1000),
             millones = paste(secuencia_ingresos/1000000)) |> 
      mutate(secuencia_ingresos_etiqueta = case_when(#secuencia_ingresos < secuencia_salto ~ paste("Menos de", secuencia_salto/1000, "mil"),
        secuencia_ingresos < secuencia_salto() ~ paste("0"),
        # secuencia_ingresos < 1000000 & lead(secuencia_ingresos) == 1000000 ~ paste(miles, "a", lead(millones), "millón"),
        secuencia_ingresos < 1000000 ~ paste(miles, "mil"),
        secuencia_ingresos == 1000000 ~ paste(millones, "millón"),
        secuencia_ingresos == secuencia_maximo() ~ paste(secuencia_maximo()/1000000, "millones o más"),
        secuencia_ingresos > 1000000 ~ paste(millones, "millones")
      )) |> 
      pull(secuencia_ingresos_etiqueta)
  })
  
  
  #gráfico densidad ----
  output$grafico_densidad <- renderPlot({
    req(datos_densidad())
    req(input$detalle != "")
    
    # browser()
    # dev.new()
    
    .variable_color = "comuna"
    
    #nivel de detalle de palabras a números
    .detalle <- switch(input$detalle,
                       "Bajo" = 2,
                       "Normal" = 1,
                       "Alto" = 0.7)
    
    n_comunas <- length(input$comunas)
    
    .transparencia <- case_when(
      n_comunas <= 3 ~ 0.7,
      n_comunas == 4 ~ 0.5,
      n_comunas >= 5 ~ 0.4,
      .default = 0.7)
    
    datos_densidad() |>
      ggplot(aes(fill = .data[[.variable_color]])) +
      # geom_density(aes(variable), alpha = 0.5, linewidth = 0, adjust = .detalle) +
      geom_density(aes(x = variable, weight = expc, y = after_stat(density)),
                   alpha = .transparencia, linewidth = 0, adjust = .detalle) +
      # geom_vline(aes(xintercept = 500000)) +
      scale_x_continuous(breaks = seq_ingresos(),
                         labels = secuencia_ingresos_etiqueta(),
                         expand = expansion(0)) +
      scale_fill_brewer(palette = "Dark2") +
      scale_y_continuous(expand = expansion(c(0, 0.1))) +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
      #fondo
      theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
            plot.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.key = element_rect(fill = color_fondo)) +
      #leyenda
      theme(legend.position = "bottom", legend.direction = "horizontal") +
      #otros
      theme(axis.line.x = element_line(linewidth = 2, color = color_detalle, lineend = "round"),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_line(linewidth = 0.5, color = color_detalle),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(color = color_texto, size = 13),
            axis.text.y = element_blank(),
            axis.text.x = element_text(margin = margin(t = 5, b = -20)),
            legend.text = element_text(color = color_texto, size = 13, 
                                       margin = margin(t= 4, b = 4, l = 6, r = 20)),
            # legend.background = element_blank(),
            legend.title = element_blank(),
            axis.title = element_blank(),
            plot.margin = unit(c(1, 4, 1, 1), "mm")
      )
  })
  
  # gráfico densidad 2 ----
  
  output$grafico_densidad_2 <- renderPlot({
    req(datos_densidad())
    req(input$detalle != "")
    # browser()
    
    datos_densidad() |>
      ggplot(aes(x = variable, y = comuna, 
                 fill = comuna, col = comuna)) +
      geom_density_ridges(rel_min_height = 0.001, size = 2,
                          alpha = .3, scale = 1.9) +
      geom_text(aes(label = paste0("  ", comuna), 
                    x = max(seq_ingresos())), 
                check_overlap = T, 
                hjust = 0, vjust = 0, size = 4.5) +
      scale_x_continuous(
        breaks = seq_ingresos(),
        labels = secuencia_ingresos_etiqueta(),
        expand = expansion(0), 
        limits = c(-1000, max(seq_ingresos()) + 1000)) +
      scale_y_discrete(expand = expansion(c(0, 0.1))) +
      scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "color")) +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
      coord_cartesian(clip = "off") +
      #fondo
      theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
            plot.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.key = element_rect(fill = color_fondo)) +
      #otros
      theme(axis.line.x = element_line(linewidth = 2, color = color_detalle, lineend = "round"),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_line(linewidth = 0.5, color = color_detalle),
            panel.grid.major.y = element_line(linewidth = 1, color = color_detalle),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(color = color_texto, size = 13),
            axis.text.y = element_blank(),
            axis.text.x = element_text(margin = margin(t = 5, b = -20)),
            legend.title = element_blank(),
            axis.title = element_blank(),
            plot.margin = unit(c(4, 30, 10, 0), "mm")
      )
  })
  
  # gráfico dispersión ----
  output$grafico_dispersion <- renderPlot({
    req(datos_dispersion())

    # browser()
    datos_dispersion() |> 
      ggplot(aes(x = variable, y = 1,
                 # fill = comuna_seleccionada, color = comuna_seleccionada, 
                 size = comuna_seleccionada, alpha = comuna_seleccionada)) +
      geom_jitter(data = datos_dispersion() |> filter(!comuna_seleccionada), 
                  color = color_secundario, width = 0, height = 1) +
      #línea vertical de promedio
      # geom_vline(xintercept = mean(datos_dispersion()$variable), 
      #            linewidth = 1.2, linetype = "solid", 
      #            color = color_fondo) +
      #sombra de puntos de comuna
      geom_point(data = datos_dispersion() |> filter(comuna_seleccionada), 
                 aes(color = comuna),
                 size = 11, color = color_fondo, alpha = 0.6) +
      #puntos de comuna
      geom_point(data = datos_dispersion() |> filter(comuna_seleccionada), 
                 aes(color = comuna)) +
      #tamaño de puntos destacados
      scale_size_manual(values = c(5, 10), guide = "none") +
      #transparencia de puntos destacados
      scale_alpha_manual(values = c("TRUE" = 0.7, "FALSE" = 0.2), guide = "none") +
      scale_y_continuous(limits = c(0, 2)) +
      scale_color_brewer(palette = "Dark2") +
      scale_x_continuous(label = ~format(.x, big.mark = ".", decimal.mark = ","),
                         expand = expansion(0.05)) +
      #fondo
      theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
            plot.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.key = element_rect(fill = color_fondo)) +
      #otros
      theme(#axis.line.x = element_line(linewidth = 2, color = color_detalle, lineend = "round"),
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
      ) +
      theme(legend.position = "none")
  })
  
  # tablas ----
  
  datos_ingresos <- reactive({
    datos <- datos() |> 
      select(-variable) |> 
      mutate(ytotcorh = if_else(pco1 == "1. Jefatura de Hogar", ytotcorh, NA),
             ypc = if_else(pco1 == "1. Jefatura de Hogar", ypc, NA)) |> 
      group_by(comuna) |> 
      mutate(across(where(is.numeric), as.double))
    
    medianos <- datos |> 
      summarize(across(c(where(is.numeric), -expc), ~weighted.median(.x, w = expc, na.rm = T)))
    
    promedios <- datos |> 
      summarize(across(c(where(is.numeric), -expc), ~weighted.mean(.x, w = expc, na.rm = T)))
    
    return(list("medianos" = medianos, "promedios" = promedios))
  })
  
  
  output$tabla_ingresos_medianos <- render_gt({
    # browser()
    
    datos_ingresos()[["medianos"]] |> 
      arrange(desc(ytotcor)) |> 
      gt() |> 
      fmt_number(columns = where(is.numeric), sep_mark = ".", decimals = 0) |> 
      cols_label(
        ytotcorh = "Ingresos de los hogares",
        ytotcor = "Ingresos individuales",
        yoprcor = "Ingreso de la ocupación principal",
        ypc = "Ingreso per cápita de hogares",
        y2803 = "Jubilación o pensión por vejez",
        comuna = "Comuna"
      ) |> 
      tab_style(locations = cells_column_labels(),
                style = cell_text(weight = "bold")) |> 
      tab_options(table.font.color = color_texto, table.font.color.light = color_texto, 
                  table_body.hlines.color = color_detalle,
                  table_body.vlines.color = color_detalle,
                  column_labels.border.top.color = color_fondo, column_labels.border.bottom.color = color_detalle, 
                  table_body.border.bottom.color = color_detalle,
                  table.background.color = color_fondo)
  })
  
  output$tabla_ingresos_promedio <- render_gt({
    # browser()
    
    datos_ingresos()[["promedios"]] |> 
      arrange(desc(ytotcor)) |> 
      gt() |> 
      fmt_number(columns = where(is.numeric), sep_mark = ".", decimals = 0) |> 
      cols_label(
        ytotcorh = "Ingresos de los hogares",
        ytotcor = "Ingresos individuales",
        yoprcor = "Ingreso de la ocupación principal",
        ypc = "Ingreso per cápita de hogares",
        y2803 = "Jubilación o pensión por vejez",
        comuna = "Comuna"
      ) |> 
      tab_style(locations = cells_column_labels(),
                style = cell_text(weight = "bold")) |> 
      tab_options(table.font.color = color_texto, table.font.color.light = color_texto, 
                  table_body.hlines.color = color_detalle,
                  table_body.vlines.color = color_detalle,
                  column_labels.border.top.color = color_fondo, column_labels.border.bottom.color = color_detalle, 
                  table_body.border.bottom.color = color_detalle,
                  table.background.color = color_fondo)
  })
  
}


shinyApp(ui = ui, server = server)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(stringr)
library(fresh)
library(shinycssloaders)
library(glue)

options(scipen=999)

color_fondo = "#222222"
color_secundario = "#333333"
color_detalle = "#333333"
color_texto = "#999999"
color_destacado = "#BBBBBB"

# diccionario de variables
# source("variables.R")

#cargar datos ----
# casen_comunas <- readr::read_csv2("casen_pobreza_comunas.csv", 
#                                   col_types = c(rep("c", 3), rep("n", 44))
# )
# casen_comunas <- arrow::read_feather("app/casen_ingresos.feather")
casen_comunas <- arrow::read_feather("casen_ingresos.feather")
variables_numericas <- casen_comunas |> select(where(is.numeric)) |> names()


# funciones ----
#se usa para transformar el nombre de la variable (desde los inputs) a la etiqueta
nombre_variable <- function(texto) {
  # texto = input$selector_y
  unlist(variables)[unlist(variables) == texto] |> names() |> str_remove(".*\\.")
}

estilo_cuadros <- glue("margin: -6px; margin-top: 10px; margin-bottom: 10px;
                           padding: 20px;
                           padding-top: 0;
                           border-radius: 8px;
           border: 3px solid {color_detalle} ;")

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
                   link_color = color_destacado),
    bs_vars_font(size_base = "16px", #aumentar globalmente tamaño de letra  
                 # family_sans_serif = "Urbanist" #cargar fuente o tipo de letra
    ), 
    bs_vars_modal(content_bg = color_fondo, content_border_color = color_detalle, 
                  backdrop_bg = color_fondo, backdrop_opacity = "60%"),
    bs_vars_button(
      default_color = color_fondo,
      default_bg = color_destacado,
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
    color: ", color_destacado, ";
    }")),
  
  tags$style(paste0("
    strong {
    font-weight: bold;
    color: ", color_destacado, ";
    }")),
  
  #labels de inputs
  tags$style(paste0("
    h4 {
    font-style: italic;
    font-size: 100%;
    margin-bottom: -16px;
    margin-top: 0px;
    font-family: Aleo;
    color: ", color_destacado, ";
    }")),
  
  #enlaces
  tags$style(paste0("
    a {
    text-decoration: underline;
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
         background: ", color_destacado, " !important;
         }
  
         .dropdown-header {
         color: black !important;
         font-family: Aleo;
         font-weight: bold;
         font-size: 110%;
         }
         
         .form-control {
         color: ", color_texto, " !important;
         box-shadow: none;
         }
         
         .no-results {
         color: black !important;
         background: ", color_destacado, " !important;
         }
         
         .selected {
         background-color: ", color_secundario, " !important;
         color: ", color_destacado, " !important;
         }")),
  
  #botones, botones hover
  tags$style(paste0("
    .action-button {
    opacity: 0.6; font-size: 80%; padding: 4px; padding-left: 8px; padding-right: 8px; color: black; 
    border: 3px solid", color_destacado, ";
    }
    .action-button:hover, .action-button:active, .action-button:focus {
    opacity: 1;
    color: black; 
    border: 3px solid", color_destacado, ";
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
  background-color: ", color_destacado, ";
  box-shadow: none;
  border: 3px solid ", color_detalle, ";
  height: 30px; width: 30px;
  }
  
  /*pelota de slider en hover o activa*/
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
           p("La visualización indica en qué tramos de ingresos se ubican las personas de la comuna seleccionada, donde la altura de la curva representa a una mayor proporción de las personas que perciben los ingresos que indica el eje horizontal.
           Por ejemplo, un gráfico con mucha altura en la parte inferior de la escala (izquierda) significa que la mayoría de la población percibe ingresos bajos, o un gráfico con mucha altura simultáneamente en los extremos izquierdo y derecho representaría una población con alta desigualdad y polarización de ingresos."),
           p("Los datos de este visualizador provienen de la",
             tags$a("Encuesta de caracterización socioeconómica nacional (Casen) 2022.", target = "_blank", href = "https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2022"))
           
    )
  ),
  
  
  # selectores ----
  
  fluidRow(
    column(12,
           hr(),
           h3("Seleccionar variables")
    )
  ),
  fluidRow(
    column(5, 
           p("Selecciona una comuna para ver su distribución de ingresos, o elige varias comunas para comparar los ingresos de sus poblaciones."),
           column(12, align = "center",
                  div(style = "max-width: 350px; min-width: 290px;",
                      radioGroupButtons(
                        inputId = "variable",
                        label = NULL,
                        width = "100%",  justified = T,
                        # choices = c("Ingresos individuales", "Ingresos de los hogares")
                        direction = "vertical",
                        choices = c("Ingresos individuales" = "ytotcor",
                                    "Ingresos de los hogares" = "ytotcorh",
                                    "Ingresos por persona de hogares" = "ypc",
                                    "Jubilación o pensión por vejez" = "y2803")
                      )
                  )
           )
    ),
    column(7, style = "padding: 0;",
           column(12,
                  p("Elija una o más regiones para posteriormente elegir una o varias comunas que serán incluidas en el gráfico."),
                  pickerInput("regiones",
                              label = h4("Regiones"),
                              width = "100%",
                              multiple = TRUE,
                              selected = "Región Metropolitana de Santiago", 
                              # choices = NULL,
                              choices = c("Todas las regiones", as.character(unique(casen_comunas$region))),
                              options = list(width = FALSE)
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
                  )
           )
    )
  ),
  
  
  
  
  
  #grafico ----
  fluidRow(
    column(12,
           hr(),
           h3("Visualizar"),
           
    ),
    column(12, align = "center", style = "padding: 10px;",
           plotOutput("grafico", width = "100%", height = 600) |> 
             withSpinner(color = color_destacado, type = 8)
    )
  ),
  
  # opciones del gráfico ----
  fluidRow(
    column(12,
           hr()
    ),
    column(12, align = "center", 
           div(
             
             style = "max-width: 400px;",
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
           )
    )
  ),
  
  # firma ----
  fluidRow(
    column(12,
           hr(),
           p("Diseñado y programado por",
             tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
           p(
             "Código de fuente de esta app y del procesamiento de los datos",
             tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/casen_comparador_ingresos")
           ),
           div(style = "height: 40px")
           
    )
  )
  
)


#—----

server <- function(input, output, session) {
  
  #selectores ----

    #filtrar comunas según regiones elegidas
  lista_comunas <- reactive({
    if ("Todas las regiones" %in% input$regiones) {
      lista_comunas <- split(unique(casen_comunas$comuna), 
                             unique(casen_comunas$region))
    } else {
      casen_region <- casen_comunas |> 
        filter(region == input$regiones)
      lista_comunas <- split(unique(casen_region$comuna), 
                             unique(casen_region$region))
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
                                     noneSelectedText = "Sin selección")
    )
  })
  
  #comunas al azar
  observeEvent(input$azar_comunas, {
    azar_comunas <- sample(unlist(lista_comunas()), 4)
    updatePickerInput(session,
                      inputId = "comunas",
                      selected = azar_comunas
    )
  })
  
  
  # datos ----
  datos <- reactive({
    req(length(input$comunas) > 0)
    
    # .variable <- switch(input$variable,
    #                     "Ingresos individuales" = "ytotcor",
    #                     "Ingresos de los hogares" = "ytotcorh"
    # )
    
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
      mutate(variable = !!sym(input$variable)) |> 
      #limitar máximo
      mutate(variable = ifelse(variable >= secuencia_maximo(), 
                               secuencia_maximo(), 
                               variable))
    
    return(dato2)
  })
  
  
  #máximo de ingresos ----
  observeEvent(input$variable, {
    maximo_por_variable = switch(input$variable,
                                 "y2803" = 2000000, #pensiones
                                 "ytotcorh" = 8000000, #hogares
                                 "ytotcor" = 2000000, #personas
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
  
  
  #gráfico ----
  output$grafico <- renderPlot({
    req(datos())
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
    
    datos() |>
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
            axis.text.x = element_text(margin = margin(t = 5)),
            legend.text = element_text(color = color_texto, size = 13, margin = margin(t= 4, b = 4, r = 14)),
            # legend.background = element_blank(),
            legend.title = element_blank(),
            axis.title = element_blank()
      )
  })
}


shinyApp(ui = ui, server = server)
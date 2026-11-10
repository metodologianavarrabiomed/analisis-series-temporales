# =============================================================================
# APP SHINY FINAL — DASHBOARD + FILTRO + INFORME DESCARGABLE
# =============================================================================

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(lubridate)
library(shinycssloaders)
library(rmarkdown)
library(plotly)
library(zoo)

# ---- MOTOR ----
source("analisis_automatizado.R") # Debe contener analisis_series_temporales() y analisis_ratios_series()

options(error = function() {
  traceback(2)
})

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Series Temporales"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Series individuales", tabName = "series", icon = icon("chart-line")),
      menuItem("Ratios", tabName = "ratios", icon = icon("percentage")),
      menuItem("Ayuda", tabName = "ayuda", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
    .content-wrapper { background-color: #f5f7fa; }
    .box { border-radius: 8px; }
    .required-star { color: #e53935; font-weight: 700; margin-left: 4px; }

    .shiny-input-container { margin-bottom: 0 !important; }
    .form-group { margin-bottom: 0 !important; }
    .input-group { margin-bottom: 0 !important; }

    /* ESTA ES LA CLAVE */
    .shiny-file-input {
      margin-bottom: 0 !important;
      padding-bottom: 0 !important;
    }

    .help-text {
      margin-top: 4px;
      margin-bottom: 16px;
      display: block;
    }

    .box .btn,
    .box.fullscreen .btn {
      width: 100% !important;
      display: block;
    }
  "))
    ),
    tags$head(
      tags$script(HTML("
    setInterval(function() {
      Shiny.setInputValue('keepalive', new Date().getTime(), {priority: 'event'});
    }, 5 * 60 * 1000);
  "))
    ),
    
    tabItems(
      
      # ================= SERIES =================
      tabItem(tabName = "series",
              fluidRow(
                box(title = "Configuración", width = 12, status = "primary", solidHeader = TRUE,
                    fluidRow(
                      # ================= FILA 1 =================
                      column(4,
                             fileInput(
                               "archivo",
                               tagList(
                                 "Cargar Excel (.xlsx)",
                                 span("*", class = "required-star"),
                                 tags$br(),
                                 tags$small(
                                   class = "text-muted",
                                   "Columnas requeridas: fecha, tipo (tipo de medicamento) y pacientes (número)."
                                 )
                               )
                             )
                      ),
                      
                      column(
                        4,
                        dateInput(
                          "fecha_intervencion_s",
                          tagList("Fecha intervención", span("*", class="required-star"))
                        )
                      ),
                      
                      column(
                        4,
                        dateInput(
                          "inicio_obs_s",
                          "Inicio observación (opcional)"
                        ),
                        tags$small(
                          class = "text-muted help-text",
                          "Fecha desde la que se evalúa el efecto de la intervención.",
                          tags$br(),
                          "Si no se indica, se usa la fecha de intervención."
                        )
                      )
                    ),
                    
                    # ================= FILA 2 =================
                    fluidRow(
                      column(4, uiOutput("ui_tipos_filtrado")),  # columna vacía para alinear
                      
                      column(
                        4,
                        selectInput(
                          "modo_modelo_s",
                          tagList("Modo", span("*", class="required-star")),
                          choices = c("Individual" = "individual", "Global" = "global")
                        )
                      ),
                      
                      column(
                        4,
                        dateInput(
                          "fin_obs_s",
                          "Fin observación (opcional)"
                        ),
                        tags$small(
                          class = "text-muted help-text",
                          "Fecha hasta la que se evalúa el efecto de la intervención.",
                          tags$br(),
                          "Si no se indica, se usa la última fecha disponible en los datos."
                        )
                      )
                    ),
                    
                    # ================= FILA 3 (BOTONES ALINEADOS POR COLUMNA) =================
                    fluidRow(
                      column(4),  # columna vacía (alineación con "Archivo")
                      
                      column(
                        4,
                        actionButton(
                          "run_series",
                          "▶ Analizar",
                          class = "btn-primary btn-lg",
                          width = "100%"
                        )
                      ),
                      
                      column(
                        4,
                        downloadButton(
                          "descargar_informe_series",
                          "Descargar informe",
                          class = "btn-success btn-lg",
                          width = "100%",
                          style = "height: 45px; font-size: 18px;"
                        )
                      )
                    )
                    
                )
              ),
              fluidRow(
                box(width = 12,
                    withSpinner(uiOutput("series_output"))
                )
              )
      ),
      
      # ================= RATIOS =================
      tabItem(tabName = "ratios",
              fluidRow(
                box(title = "Configuración ratio", width = 12, status = "info", solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             uiOutput("ui_tipos_num"),
                             uiOutput("ui_tipos_den")
                      ),
                      column(4,
                             dateInput("fecha_intervencion_r", tagList("Fecha intervención", span("*", class="required-star"))),
                             dateInput("inicio_obs_r", "Inicio observación"),
                             dateInput("fin_obs_r", "Fin observación")
                      ),
                      column(4,
                             selectInput("modo_modelo_r", tagList("Modo", span("*", class="required-star")),
                                         choices = c("Individual"="individual","Global"="global")),
                             br(),
                             actionButton("run_ratio", "▶ Analizar ratio",
                                          class = "btn-primary btn-lg", width = "100%"),
                             br(), br(),
                             downloadButton("descargar_informe_ratio", "Descargar informe",
                                            class = "btn-success btn-lg", width = "100%",
                                            style = "height: 45px; font-size: 18px; width: 525px;")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                    withSpinner(uiOutput("ratio_output"))
                )
              )
      ),
      
      # ================= AYUDA =================
      tabItem(tabName = "ayuda",
              fluidRow(
                box(title = "Documentación y uso", width = 12, status = "info",
                    solidHeader = TRUE,
                    
                    h4(tags$b("Formato del fichero de datos")),
                    p("El fichero Excel debe contener exactamente estas tres columnas:"),
                    tags$ul(
                      tags$li(tags$b("fecha"), " — Fecha semanal (formato YYYY-MM-DD)"),
                      tags$li(tags$b("tipo"),  " — Nombre del tipo de medicamento (texto)"),
                      tags$li(tags$b("pacientes"), " — Valor numérico semanal")
                    ),
                    
                    h4(tags$b("Metodología")),
                    p("El sistema dispone de cuatro modelos de predicción para construir el contrafactual (“¿qué habría ocurrido sin la intervención?”). Estos modelos pueden seleccionarse de dos maneras:"),
                    tags$ul(
                      tags$li(tags$b("Modo Global:"), " se calcula el mejor modelo dentro de cada uno de los cuatro enfoques, se generan sus métricas, se obtiene un ranking y se elige el modelo ganador. Ese modelo único se aplica a todas las series (todos los fármacos)."),
                      tags$li(tags$b("Modo Individual:"), " para cada tipo de fármaco se evalúan los cuatro modelos por separado y se selecciona el que presenta el mejor rendimiento de manera independiente para esa serie.")
                    ),
                    p("Todos los modelos se entrenan exclusivamente con datos previos a la fecha de intervención y luego se proyectan hacia adelante como contrafactual."),
                    
                    p(tags$b("ARIMA no estacional"), " con regresores externos:"),
                    tags$ul(
                      tags$li("Términos de Fourier (pares seno/coseno) para capturar la estacionalidad semanal (frecuencia = 52)"),
                      tags$li("Tendencia lineal (variable t = 1, 2, …, n)"),
                      tags$li("El número óptimo de términos K (1–10) se selecciona minimizando el AICc")
                    ),
                   
                    p(tags$b("Prophet")),
                    p("Prophet es un modelo aditivo desarrollado para capturar automáticamente tendencias, estacionalidades y cambios estructurales. Sus componentes principales son:"),
                    tags$ul(
                      tags$li("Tendencia automática con detección de changepoints."),
                      tags$li("Estacionalidad semanal y anual incluida por defecto mediante funciones de Fourier."),
                      tags$li("Festivos o eventos externos si se especifican.")
                    ),
                    p("Es especialmente robusto ante faltantes, cambios abruptos y patrones complejos."),
                    
                    p(tags$b("STL + ARIMA")),
                    p("Este enfoque separa explícitamente la serie en componentes mediante descomposición STL:"),
                    tags$ul(
                      tags$li("S: componente estacional suave, extraída con STL."),
                      tags$li("T: tendencia estimada por descomposición."),
                      tags$li("R: componente residual.")
                    ),
                    p("Posteriormente, sobre el residuo R se ajusta un modelo ARIMA, que captura autocorrelación y dinámica temporal."),
                    p("El contrafactual se construye combinando:"),
                    tags$ul(
                      tags$li("estacionalidad proyectada"),
                      tags$li("tendencia suavizada"),
                      tags$li("predicción ARIMA del residual")
                    ),
                    
                    p(tags$b("STL + ETS")),
                    p("El procedimiento comienza igual, aplicando descomposición STL para separar la serie en estacionalidad, tendencia y residuo."),
                    p("En este caso, el residuo R se modela mediante un modelo ETS (Error–Trend–Seasonal), que permite:"),
                    tags$ul(
                      tags$li("errores aditivos o multiplicativos"),
                      tags$li("tendencias aditivas/multiplicativas con o sin amortiguación"),
                      tags$li("modelado flexible de la dinámica de nivel y crecimiento")
                    ),
                    p("El resultado final es una predicción contrafactual construida con estacionalidad STL + tendencia suavizada + pronóstico ETS del residuo."),
                    
                    h4(tags$b("Interpretación del gráfico")),
                    tags$ul(
                      tags$li(tags$span(style = "color:grey", "■"), " Gris: serie observada"),
                      tags$li(tags$span(style = "color:#83D0BB", "■"), " Verde sólido: ajuste del modelo"),
                      tags$li(tags$span(style = "color:#83D0BB", "- -"), " Verde punteado: predicción contrafactual"),
                      tags$li(tags$span(style = "color:red", "│"), " Rojo: fecha de intervención")
                    ),
                    
                    h4(tags$b("Ratios")),
                    p("Se calcula el ratio semanal = Σ(numerador) / Σ(denominador) y se modela tanto",
                      " la serie original como su media móvil de orden 4."),
                    
                    h4(tags$b("Tablas de resultados")),
                    p("Se evalúa el período definido por inicio/fin de observación:"),
                    tags$ul(
                      tags$li(tags$b("Diferencia"), " = Observado − Predicho"),
                      tags$li(tags$b("Caída %"), " = (Diferencia / Predicho) × 100")
                    )
                
           )   )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  observe({
    invalidateLater(5 * 60 * 1000, session)
  })
  
  
  observeEvent(input$keepalive, {
    invisible(NULL)
  })
  
  
  # ---- reactiveVals para guardar resultados ----
  resultado_series <- reactiveVal(NULL)
  resultado_ratios <- reactiveVal(NULL)
  
  # ---- outputs vacíos ----
  output$series_output <- renderUI({ NULL })
  output$ratio_output  <- renderUI({ NULL })
  
  # ---------------- SELECTOR FÁRMACOS ----------------
  observeEvent(input$archivo, {
    df <- read_excel(input$archivo$datapath)
    names(df) <- tolower(names(df))
    validate(need("tipo" %in% names(df), "El Excel debe tener columna 'tipo'"))
    tipos <- unique(df$tipo)
    
    # Series
    output$ui_tipos_filtrado <- renderUI({
      checkboxGroupInput("tipos_filtrado", "Fármacos a analizar",
                         choices = tipos, selected = tipos)
    })
    
    # Ratios
    output$ui_tipos_num <- renderUI({
      selectInput("tipos_num", tagList("Numerador", span("*", class="required-star")),
                  choices = tipos, selected = tipos[1], multiple = TRUE)
    })
    output$ui_tipos_den <- renderUI({
      selectInput("tipos_den", tagList("Denominador", span("*", class="required-star")),
                  choices = tipos, selected = if(length(tipos)>1) tipos[2] else tipos[1], multiple = TRUE)
    })
  })
  
  # ---------------- DATOS ----------------
  datos_raw <- reactive({
    req(input$archivo)
    df <- read_excel(input$archivo$datapath)
    names(df) <- tolower(names(df))
    validate(need(all(c("fecha","tipo","pacientes") %in% names(df)),
                  "Columnas necesarias: fecha, tipo, pacientes"))
    
    df <- df %>% mutate(
      fecha = as.Date(parse_date_time(fecha,
                                      orders = c("ymd", "dmy", "mdy")))
    )
    
    if (!is.null(input$tipos_filtrado)) df <- df %>% filter(tipo %in% input$tipos_filtrado)
    df
  })
  
  # ================= SERIES =================
  observeEvent(input$run_series, {
    req(datos_raw())
    output$series_output <- renderUI({ div(style="text-align:center; padding:40px;", h3("Procesando...")) })
    resultado <- tryCatch({
      
      # 💥 ESTA ES LA LÍNEA QUE DEBES AÑADIR:
      analisis_sensibilidad_consola(datos_raw(), input$fecha_intervencion_s)
      
      analisis_series_temporales(
        datos = datos_raw(),
        fecha_intervencion = input$fecha_intervencion_s,
        inicio_observacion = input$inicio_obs_s,
        fin_observacion    = input$fin_obs_s,
        modo = input$modo_modelo_s
      )
    }, error = function(e){ div(style="color:red;", paste("Error:", e$message)) })
    resultado_series(resultado)
    output$series_output <- renderUI({ resultado })
  })
  
  # ---------------- DESCARGA INFORME SERIES ----------------
  output$descargar_informe_series <- downloadHandler(
    filename = function(){ paste0("informe_series_", Sys.Date(), ".html") },
    content = function(file){
      req(resultado_series())
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("plantilla_informe.Rmd", temp_rmd, overwrite = TRUE)
      rmarkdown::render(
        temp_rmd, output_file = file,
        params = list(
          informe = resultado_series(),
          fecha_intervencion = input$fecha_intervencion_s,
          inicio_obs = input$inicio_obs_s,
          fin_obs = input$fin_obs_s,
          modo = input$modo_modelo_s
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # ================= RATIOS =================
  observeEvent(input$run_ratio, {
    req(datos_raw(), input$tipos_num, input$tipos_den)
    output$ratio_output <- renderUI({ div(style="text-align:center; padding:40px;", h3("Procesando ratio...")) })
    resultado <- tryCatch({
      analisis_ratios_series(
        datos = datos_raw(),
        numerador_tipos = input$tipos_num,
        denominador_tipos = input$tipos_den,
        fecha_intervencion = input$fecha_intervencion_r,
        inicio_observacion = input$inicio_obs_r,
        fin_observacion    = input$fin_obs_r,
        modo = input$modo_modelo_r
      )
    }, error = function(e){ div(style="color:red;", paste("Error:", e$message)) })
    resultado_ratios(resultado)
    output$ratio_output <- renderUI({ resultado })
  })
  
  # ---------------- DESCARGA INFORME RATIOS ----------------
  output$descargar_informe_ratio <- downloadHandler(
    filename = function(){ paste0("informe_ratios_", Sys.Date(), ".html") },
    content = function(file){
      req(resultado_ratios())
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("plantilla_informe.Rmd", temp_rmd, overwrite = TRUE)
      rmarkdown::render(
        temp_rmd, output_file = file,
        params = list(
          informe = resultado_ratios(),
          fecha_intervencion = input$fecha_intervencion_r,
          inicio_obs = input$inicio_obs_r,
          fin_obs = input$fin_obs_r,
          modo = input$modo_modelo_r
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# =============================================================================
# RUN APP
# =============================================================================
shinyApp(ui, server)
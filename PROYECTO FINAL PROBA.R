# Instalar y cargar las librerías necesarias
if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("DT")) install.packages("DT")

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Cargar los datos desde un archivo .txt o .csv
datos <- read.table(file.choose(), header = TRUE, sep = ",")

# Convertir las variables a numéricas si es necesario
datos <- datos %>%
  mutate(across(c(DLHRWAGE, DEDUC1, AGE, AGESQ, HRWAGEH, WHITEH, MALEH, EDUCH, 
                  HRWAGEL, WHITEL, MALEL, EDUCL, DEDUC2, DTEN, DMARRIED, DUNCOV), 
                ~ as.numeric(as.character(.))))

# Eliminar registros con datos faltantes o inválidos
datos_sin_faltantes <- datos[rowSums(is.na(datos) | datos == ".") == 0, ]

# Funciones para calcular medidas estadísticas
calcular_estadisticas <- function(datos, variable) {
  media <- mean(datos[[variable]], na.rm = TRUE)
  mediana <- median(datos[[variable]], na.rm = TRUE)
  moda <- as.numeric(names(sort(table(datos[[variable]]), decreasing = TRUE)[1]))
  rango <- range(datos[[variable]], na.rm = TRUE)
  varianza <- var(datos[[variable]], na.rm = TRUE)
  desviacion_estandar <- sd(datos[[variable]], na.rm = TRUE)
  cv <- (desviacion_estandar / media) * 100
  q1 <- quantile(datos[[variable]], 0.25, na.rm = TRUE)
  q2 <- quantile(datos[[variable]], 0.50, na.rm = TRUE)
  q3 <- quantile(datos[[variable]], 0.75, na.rm = TRUE)
  
  data.frame(
    Medida = c("Media (USD)", "Mediana (USD)", "Moda (USD)", "Rango (USD)", 
               "Varianza (USD^2)", "Desviación Estándar (USD)", "CV (%)", 
               "Q1 (USD)", "Q2 (USD)", "Q3 (USD)", 
               "Mínimo (USD)", "Máximo (USD)"),
    Valor = round(c(media, mediana, moda, diff(rango), varianza, desviacion_estandar, cv, 
                    q1, q2, q3, rango[1], rango[2]), 2)
  )
}

# Crear la aplicación Shiny
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f3e5f5;
      }
      .tabbable > .nav > li > a {
        color: #4a148c;
        background-color: #e1bee7;
      }
      .tabbable > .nav > li[class=active] > a {
        background-color: #7E57C2;
        color: white;
      }
      h1, h2, h3, h4, h5 {
        color: #4a148c;
      }
      .well {
        background-color: #ede7f6;
        border: 1px solid #7E57C2;
      }
      .dataTables_wrapper .dataTables_length select {
        color: #4a148c;
      }
      .dataTables_wrapper .dataTables_filter input {
        color: #4a148c;
      }
      .dataTables_wrapper .dataTables_info {
        color: #4a148c;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #4a148c;
      }
    "))
  ),
  titlePanel("Reporte"),
  tabsetPanel(
    tabPanel("Análisis de Datos",
             fluidRow(
               column(12, wellPanel(
                 h4("Resumen de los Datos"),
                 verbatimTextOutput("analisis_datos")
               ))
             ),
             fluidRow(
               column(12, wellPanel(
                 h4("Tabla de Datos Sin Registros Faltantes"),
                 DT::dataTableOutput("tabla_sin_faltantes")
               ))
             )
    ),
    tabPanel("Gráfica",
             fluidRow(
               column(12, wellPanel(
                 h4("Densidad de Kernel Bivariada de Salarios por Hora"),
                 p("Esta gráfica muestra la densidad de kernel bivariada de los salarios por hora de dos gemelos."),
                 p("Las variables están medidas en dólares estadounidenses."),
                 plotOutput("grafica_kernel"),
                 p("El gráfico de densidad de kernel bivariada ayuda a visualizar la distribución conjunta de dos variables continuas.
                   En este caso, se observa la relación entre los salarios por hora de dos gemelos, donde se pueden identificar regiones con mayor densidad de puntos.
                   Esto proporciona una comprensión más profunda de cómo se distribuyen los salarios y si existe alguna correlación entre ellos."),
                 p("El eje x representa el salario por hora del gemelo 1 en dólares estadounidenses (USD), mientras que el eje y representa el salario por hora del gemelo 2 en USD.
                   Los puntos en la gráfica indican observaciones individuales, y las líneas de contorno muestran las áreas de mayor densidad, donde se encuentran más puntos."),
                 p("La gráfica también incluye un relleno de densidad, donde los colores más oscuros indican regiones con mayor concentración de datos.
                   La presencia de líneas de contorno más cerradas indica una mayor concentración de valores en esas áreas."),
                 p("La interpretación de esta gráfica nos permite observar si los salarios por hora de los gemelos están correlacionados.
                   Si la densidad se concentra cerca de la línea diagonal que va desde el origen (0,0) hasta el punto más alto, indicaría que los salarios son similares entre los gemelos.
                   Por otro lado, si la densidad se dispersa lejos de esta línea, podría sugerir una mayor variabilidad en los salarios entre los gemelos.")
               ))
             ),
             fluidRow(
               column(12, wellPanel(
                 h4("Años de Educación por Gemelo"),
                 p("Esta gráfica muestra la densidad de kernel bivariada de los años de educación de dos gemelos."),
                 p("Las variables están medidas en años."),
                 plotOutput("grafica_educacion"),
                 p("El eje x representa los años de educación del gemelo 1, mientras que el eje y representa los años de educación del gemelo 2.
                   Los puntos en la gráfica indican observaciones individuales, y las líneas de contorno muestran las áreas de mayor densidad, donde se encuentran más puntos."),
                 p("La relación entre el nivel de educación y los salarios es un aspecto crucial a considerar en estudios económicos y sociales.
                   En general, se espera que un mayor nivel de educación esté asociado con salarios más altos. Esto se debe a que la educación tiende a aumentar las habilidades y conocimientos de una persona, haciéndola más productiva y valiosa en el mercado laboral.
                   Por lo tanto, los empleadores están dispuestos a pagar más por trabajadores más educados. Además, la educación también abre oportunidades para empleos que requieren calificaciones más altas y que generalmente pagan mejor.")
               ))
             )
    ),
    tabPanel("Estadísticas",
             fluidRow(
               column(6, wellPanel(
                 h4("Estadísticas Descriptivas: Salario por hora gemelo 1"),
                 DT::dataTableOutput("tabla_estadisticas_hrwageh")
               )),
               column(6, wellPanel(
                 h4("Estadísticas Descriptivas: Salario por hora gemelo 2"),
                 DT::dataTableOutput("tabla_estadisticas_hrwagel")
               ))
             )
    )
  )
)

server <- function(input, output) {
  # Pestaña 1: Mostrar el análisis de los datos y la tabla sin registros faltantes
  output$analisis_datos <- renderPrint({
    num_registros <- nrow(datos)
    num_variables <- ncol(datos)
    registros_con_faltantes <- sum(rowSums(is.na(datos) | datos == ".") > 0)
    registros_completos <- sum(rowSums(is.na(datos) | datos == ".") == 0)
    dim_despues <- dim(datos_sin_faltantes)
    
    cat("Número de registros:", num_registros, "\n")
    cat("Número de variables:", num_variables, "\n")
    cat("Registros con al menos un dato faltante:", registros_con_faltantes, "\n")
    cat("Registros con información completa:", registros_completos, "\n\n")
    cat("Dimensiones de la base de datos después de eliminar registros faltantes:\n")
    print(dim_despues)
  })
  
  output$tabla_sin_faltantes <- DT::renderDataTable({
    DT::datatable(datos_sin_faltantes, options = list(pageLength = 5))
  })
  
  # Pestaña 2: Mostrar la gráfica de salarios por hora
  output$grafica_kernel <- renderPlot({
    ggplot(datos_sin_faltantes, aes(x = HRWAGEH, y = HRWAGEL)) +
      geom_point(color = "black", alpha = 0.5) +  # Añadir puntos
      geom_density_2d(color = "purple") +
      labs(title = "Densidad de Kernel Bivariada",
           x = "Salario por hora gemelo 1 (USD)", 
           y = "Salario por hora gemelo 2 (USD)") +
      scale_x_continuous(limits = c(-1, NA)) +  # Establecer límite mínimo en el eje x
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#4a148c"),
            axis.title = element_text(color = "#4a148c"),
            axis.text = element_text(color = "#4a148c")) +
      geom_density_2d_filled(alpha = 0.5)
  })
  
  # Pestaña 2: Mostrar la gráfica de años de educación
  output$grafica_educacion <- renderPlot({
    ggplot(datos_sin_faltantes, aes(x = EDUCH, y = EDUCL)) +
      geom_point(color = "black", alpha = 0.5) +  # Añadir puntos
      geom_density_2d(color = "purple") +
      labs(title = "Densidad de Kernel Bivariada",
           x = "Años de educación gemelo 1", 
           y = "Años de educación gemelo 2") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#4a148c"),
            axis.title = element_text(color = "#4a148c"),
            axis.text = element_text(color = "#4a148c")) +
      geom_density_2d_filled(alpha = 0.5)
  })
  
  # Pestaña 3: Mostrar la tabla de estadísticas para gemelo 1
  output$tabla_estadisticas_hrwageh <- DT::renderDataTable({
    estadisticas_hrwageh <- calcular_estadisticas(datos_sin_faltantes, "HRWAGEH")
    DT::datatable(estadisticas_hrwageh, options = list(pageLength = 12),
                  class = 'cell-border stripe',
                  style = 'bootstrap',
                  rownames = FALSE,
                  colnames = c('Medida', 'Valor'))
  })
  
  # Pestaña 3: Mostrar la tabla de estadísticas para gemelo 2
  output$tabla_estadisticas_hrwagel <- DT::renderDataTable({
    estadisticas_hrwagel <- calcular_estadisticas(datos_sin_faltantes, "HRWAGEL")
    DT::datatable(estadisticas_hrwagel, options = list(pageLength = 12),
                  class = 'cell-border stripe',
                  style = 'bootstrap',
                  rownames = FALSE,
                  colnames = c('Medida', 'Valor'))
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

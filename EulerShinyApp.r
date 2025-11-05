
############################################################
# Método de Euler (EDO 1D): y' = f(t, y)
############################################################

# LIBRERÍA SHINY
library(shiny)

# LIBRERÍA GGPLOT
library(ggplot2)

# Función general del método de Euler
# f: función f(t, y) que devuelve la derivada
# t0: tiempo inicial
# y0: condición inicial y(t0)
# h : paso
# n : número de pasos (se generan n+1 puntos)


# EL método de Euler permite resolver una EDO de primer grado dadas las 
# condiciones iniciales x0, y0

euler_method <- function(f, t0, y0, h, n) {
  stopifnot(is.function(f), h > 0, n >= 1)
  t <- numeric(n + 1)
  y <- numeric(n + 1)
  t[1] <- t0
  y[1] <- y0
  for (k in 1:n) {
    # y_{k+1}
    y[k + 1] <- y[k] + h * f(t[k], y[k])
    t[k + 1] <- t[k] + h
  }
  data.frame(step = 0:n, t = t, y = y)
}

# EJEMPLOS :
example_functions <- list(
  # Prefefinidos 
  "y'=-2y, y(0)=1" = list(
    f = function(t, y) -2 * y,
    exact = function(t) exp(-2 * t),
    t0 = 0, y0 = 1, h = 0.1, n = 50
  ),
  "y'=r*y*(1-y/K), r=1, K=3, y(0)=0.5" = list(
    f = function(t, y) {
      r <- 1.0; K <- 3.0
      r * y * (1 - y / K)
    },
    exact = function(t) {
      r <- 1.0; K <- 3.0; y0 <- 0.5
      A <- (K - y0) / y0
      K / (1 + A * exp(-r * t))
    },
    t0 = 0, y0 = 0.5, h = 0.05, n = 200
  ),
  # Ejemplos extta (oscilantes)
  "y' = cos(t) - y" = list(
    f = function(t, y) cos(t) - y,
    exact = function(t) 0.5 * (sin(t) + cos(t) + exp(-t)),
    t0 = 0, y0 = 1, h = 0.1, n = 100
  ),
  "y' = t*y + 1" = list(
    f = function(t, y) t * y + 1,
    exact = function(t) {  # Approximation for exact
      sapply(t, function(x) {
        val <- integrate(function(s) exp(-s^2 / 2), 0, x)$value
        exp(x^2 / 2) * val
      })
    },
    t0 = 0, y0 = 0, h = 0.05, n = 100
  )
)

# Estilos y decoración 
dark_mode_css <- "
body {
  background-color: #1e1e1e;
  color: #6A0066;
  font-family: monospace;
}
h2, h3 {
  color: #E7FCFC;
}
input, select, button {
  background-color: #934790;
  color: #e0e0e0;
  border: 1px solid #FF0066;
}
table {
  color: #00ffff;
  background-color: #1e1e1e;
  border-collapse: collapse;
}
th, td {
  border: 1px solid #444;
  padding: 6px;
}
thead {
  background-color: #3b3b3b;
  color: #00ff99;
}
"

# UI
ui <- fluidPage(
  tags$head(tags$style(HTML(dark_mode_css))),
  titlePanel("Método de Euler para EDOs"),
  sidebarLayout(
    sidebarPanel(
      selectInput("example", "Selecciona un ejemplo:", choices = names(example_functions)),
      numericInput("t0", "Tiempo inicial (t0):", value = 0),
      numericInput("y0", "Condición inicial (y0):", value = 1),
      numericInput("h", "Paso (h):", value = 0.1, min = 0.001),
      numericInput("n", "Número de pasos (n):", value = 50, min = 1),
      actionButton("run", "Ejecutar"),
      verbatimTextOutput("advertencia")
    ),
    mainPanel(
      plotOutput("plot_euler"),
      plotOutput("plot_error"),
      h3("Tabla de comparación: Euler vs Exacta (primeras 20 iteraciones)"),
      tableOutput("result_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues(data = NULL, warning = NULL)
  
  # Con esto recibimos los inputs
  observeEvent(input$example, {
    ex <- example_functions[[input$example]]
    updateNumericInput(session, "t0", value = ex$t0)
    updateNumericInput(session, "y0", value = ex$y0)
    updateNumericInput(session, "h", value = ex$h)
    updateNumericInput(session, "n", value = ex$n)
  })
  
  observeEvent(input$run, {
    isolate({
      if (input$h <= 0 || input$n < 1) {
        # Advertencia si los parámetros son incorrectos
        values$warning <- "h debe ser > 0 y n >= 1."
        values$data <- NULL
        return()
      }
      # Agregamos los inputs al código de arriba
      ex <- example_functions[[input$example]]
      f <- ex$f
      exact <- ex$exact
      sol <- euler_method(f, input$t0, input$y0, input$h, input$n)
      sol$y_exact <- exact(sol$t)
      sol$error_abs <- abs(sol$y - sol$y_exact)
      values$data <- sol
      values$warning <- NULL
    })
  })
  
  # Finalmente, lo hacemos visible
  # Mostramos la advertencia (si existe)
  output$warning <- renderText({ values$warning })
  
  # TABLA DE ITERACIONES
  output$result_table <- renderTable({
    req(values$data)
    head(values$data[, c("step", "t", "y", "y_exact", "error_abs")], 20)
  })
  
  # PLOT: EULER VS EXACTA
  output$plot_euler <- renderPlot({
    req(values$data)
    sol <- values$data
    
    # Usamos la librería GGPLOT para el estilo
    ggplot(sol, aes(x = t)) +
      geom_line(aes(y = y, color = "Euler"), size = 1) +
      geom_point(aes(y = y, color = "Euler")) +
      geom_line(aes(y = y_exact, color = "Exacta"), size = 1, linetype = "dashed") +
      labs(title = paste("Euler vs Exacta -", input$example),
           x = "t", y = "y") +
      scale_color_manual(values = c("Euler" = "magenta", "Exacta" = "cyan")) +
      theme_minimal(base_family = "monospace") +
      
      # Decoración para el dark mode
      theme(
        plot.background = element_rect(fill = "#1e1e1e"),
        panel.background = element_rect(fill = "#1e1e1e"),
        text = element_text(color = "white", size = 16),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14 , color = "white", base_family = "monospace"),
        legend.key.size = unit(1.5, "lines")
      )
  })
  
  # PLOT: ERROR ABSOLUTO
  output$plot_error <- renderPlot({
    req(values$data)
    sol <- values$data
    
    # Usamos la librería GGPLOT para el estilo
    ggplot(sol, aes(x = t, y = error_abs)) +
      geom_line(color = "purple") +
      geom_point(color = "purple") +
      labs(title = "Error absoluto (Euler)", x = "t", y = "|Error|") +
      theme_minimal(base_family = "monospace") +
      theme(
        plot.background = element_rect(fill = "#1e1e1e"),
        panel.background = element_rect(fill = "#1e1e1e"),
        text = element_text(color = "white", size= 16, base_family = "monospace"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white")
      )
    
  })
}

# Inicializar la aplicación
shinyApp(ui = ui, server = server)

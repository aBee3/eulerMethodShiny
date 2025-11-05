
---
title: "Método de Euler con Shiny"
author: "Abigail Godoy Araujo A01709167, Montserrat Carrera Leal A01713040"
date: "2 de Noviembre del 2025"
output:
  pdf_document: default
  html_document:
    theme: darkly
    highlight: tango
---

# Método de Euler con Shiny

En este reporte se presenta la implementación y el análisis del método de Euler explícito para poder resolver ecuaciones diferenciales ordinarias de la forma $y'(t) = f(t, y)$. 

Se implementó la función `euler_method` en R y una aplicación en Shiny que compara la aproximación numérica con soluciones exactas en diferentes casos, donde analizamos el error absoluto y la convergencia.

---

## Introducción al método de Euler

El método de Euler es un procedimiento numérico utilizado para aproximar la solución de ecuaciones diferenciales ordinarias (EDO) cuando no es posible resolverlas de forma analítica. 

Su idea principal es avanzar paso a paso desde una condición inicial, estimando el valor siguiente mediante la epndiente local dada por la derivada $f(t, y)$, que matemáticamente se expresa como:

$$
y_{{i+1}} = y_i + h \cdot f(t_i, y_i)
$$

Donde:  
  
- $h$ es el tamaño del paso,
- $y_i$ es el valor aproximado en $t_i$,
- $f(t_i, y_i)$ es la pendiente local de la curva en ese punto.

  Este método es sencillo pero fundamental, ya que permite entender la base de otros métodos más avanzados como Runge-Kutta. Aunque es menos preciso para pasos grandes, resulta bastante útil para estudiar el comportamiento general de una solución y cómo el error depende del tamaño del paso. 

---

## Resultados: Comparación Euler vs Exacta y Error absoluto

Se aplicó el método a dos ecuaciones diferenciales distintas para comparar los resultados con sus soluciones exactas.

### Caso a)

$$
y' = -2y, \quad y(0) = 1
$$

Solución exacta:

$$
y(t) = e^{-2t}
$$

Este modelo representa una decaída exponencial. El método de Euler sigue de forma bastante precisa la curva exacta para pasos pequeños.

### Caso b)

$$
y' = ry(1 - \frac{y}{K}), \quad r=1, \quad  K=3, \quad  y(0)=0.5
$$
  
Esta es la ecuación logística, que es un modelo clásico de crecimiento poblacional con capacidad límite \( K \). En este caso, el método de Euler lo que busca es lograr aproximar la tendencia general del crecimiento, aunque se observan desviaciones más marcadas si el paso \( h \) es grande.
  
---
  
## Gráficas obtenidas con la App
Con la aplicación generamos gráficas para comparar el método de Euler con respecto a la solución exacta, que nos permite observar cómo la aproximación numérica se acerca o se aleja a la solución real según al paso h que tengamos.
  
Aunque la app Shiny no puede integrarse directamente en este markdown, las gráficas generadas incluyen:
  
- Comparación entre la solución de Euler y la exacta
- Error absoluto en función del tiempo
  
:::tip
Se incluye un folder con imágenes de la aplicación en uso.
:::
  
---

## Análisis del error y conclusiones
El error en el método de Euler se debe principalmente a la aproximación lineal que se hace en cada paso, el método asume que la pendiente $f(t,y)$ permanece constante en el intervalo de `[ti, ti+1]`, lo cual no es completamente cierto en funciones que cambian rápidamente. 
  
Mientras menor sea el paso $h$, menor el error. Cuando el paso $h$ es pequeño, los errores locales se reducen y la curva calculada se ajusta mejor a la solución exacta, pero sí $h$ aumenta, la acumulación de error en cada paso se vuelve más y más significativa. 
  
Relación observada:
  
El comportamiento del error máximo E en función del paso de h cumple con la relación de que el error es proporcional al paso h, lo que significa que si el tamaño de paso se reduce a la mitad, el error también se reduce aproximadamente a la mitad. 
    
  
$$
\text{{Error}} \propto h
$$
  
- Si $h$ se reduce a la mitad, el error también lo hace.
- La app permite comprobar visualmente esta teoría.
  
---

## Explicación del código

### Definición del método de Euler:
  
```r
euler_method <- function(f, t0, y0, h, n) {
     stopifnot(is.function(f), h > 0, n >= 1) # Advertencia
     t <- numeric(n + 1)                      # Inicializar vector t
     y <- numeric(n + 1)                      # Inicializar vector y
     t[1] <- t0                               # Condiciones iniciales
     y[1] <- y0
     for (k in 1:n) {
       # y_{k+1}
          y[k + 1] <- y[k] + h * f(t[k], y[k]) # El siguiente valor depende del anterior
          t[k + 1] <- t[k] + h                 # Actualizar t
     }
     data.frame(step = 0:n, t = t, y = y)
}
```
    
Función general del método de Euler. 
  
- $f$: función f(t, y) que devuelve la derivada.
- $t0$: tiempo inicial.
- $y0$: condición inicial y(t0).
- $h$ : tamaño del paso.
- $n$ : número de pasos (se generan n+1 puntos)
  
La función calcula la pendiente (derivada) en ese punto, y avanzando paso a paso sumando esa pendiente al valor anterior para estimar el siguiente punto.
  

### Declaración de funciones ejemplo
  
```r
example_functions <- list(
  # Prefefinidos 
  "y'=-2y, y(0)=1" = list(
    f = function(t, y) -2 * y,
    exact = function(t) exp(-2 * t),
    t0 = 0, y0 = 1, h = 0.1, n = 50  # Función con parámetros fijos
  ), ...
```

### Aplicación Shiny
  
La app permite ingresar parámetros manualmente o seleccionar ejemplos predefinidos. Utiliza `ggplot2` para visualizar:
  
- La comparación `Euler vs Exacta`
- El error absoluto

---

### Shiny

Éste código inicializa la aplicación y recibe los inputs del usuario. Además, define advertencias y muestra los plots y los elementos deseados.

```r
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
```

---

Ahora el código para que el servidor inicialice la aplicación
  
```r
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
  
  # Esperamos a que el usuario interactúe con la app
  observeEvent(input$run, {
    isolate({
    # Advertencia si los parámetros son incorrectos
      if (input$h <= 0 || input$n < 1) {
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
    # ... Código de los plots
    # ... Plot del error
```

---

### GGPLOT

Gracias a las librerías, se visualiza con colores y estilos alternativos
  
```r
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
```
  
---

## Referencias

- Chapra, S. C., & Canale, R. P. (2015). *Métodos numéricos para ingenieros*. McGraw-Hill.
- Burden, R. L., & Faires, J. D. (2011). *Análisis numérico*. Cengage Learning.
- Wickham, H. (2019). *ggplot2: Elegant Graphics for Data Analysis*. Springer.
- RStudio Team (2024). *Shiny: Web Application Framework for R*. Posit Software, PBC.

---

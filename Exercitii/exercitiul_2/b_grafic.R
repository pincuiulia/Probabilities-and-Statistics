library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  
  titlePanel("Interpretarea Geometrica a Integralei Duble"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("functie", "f(x, y):", value = "x * y"),
      numericInput("x_min", "Limita Inferioara pentru x:", value = 0),
      numericInput("x_max", "Limita Superioara pentru x:", value = 1),
      numericInput("y_min", "Limita Inferioara pentru y:", value = 0),
      numericInput("y_max", "Limita Superioara pentru y:", value = 1),
      actionButton("verifica", "Verifica")
    ),
    mainPanel(
      plotOutput("grafic_3D"),
      plotlyOutput("grafic_suprafata")
    )
  )
)

server <- function(input, output) {
  
  output$grafic_3D <- renderPlot({
    if (input$verifica > 0) {
      f <- function(x, y) {
        eval(parse(text = input$functie), list(x = x, y = y))
      }
      
      x_values <- seq(input$x_min, input$x_max, length.out = 100)
      y_values <- seq(input$y_min, input$y_max, length.out = 100)
      z_values <- outer(x_values, y_values, Vectorize(function(x, y) f(x, y)))
      
      persp(x = x_values, y = y_values, z = z_values, theta = 30, phi = 30, col = "lightblue", shade = 0.5,
            xlab = "x", ylab = "y", zlab = "f(x, y)", main = "Suprafata subgraficului")
    }
  })
  
  output$grafic_suprafata <- renderPlotly({
    if (input$verifica > 0) {
      f <- function(x, y) {
        eval(parse(text = input$functie), list(x = x, y = y))
      }
      
      x_values <- seq(input$x_min, input$x_max, length.out = 100)
      y_values <- seq(input$y_min, input$y_max, length.out = 100)
      z_values <- outer(x_values, y_values, Vectorize(function(x, y) f(x, y)))
      
      df <- data.frame(x = rep(x_values, each = length(y_values)),
                       y = rep(y_values, times = length(x_values)),
                       z = as.vector(z_values))
      
      ggplot(df, aes(x = x, y = y, z = z)) +
        geom_tile(aes(fill = z), color = "white", size = 0.5) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        labs(title = "Suprafata subgraficului", x = "x", y = "y", z = "f(x, y)") +
        theme_minimal() +
        theme(axis.title = element_text(size = 10),
              axis.text = element_text(size = 8),
              plot.title = element_text(size = 12))
    }
  })
}

shinyApp(ui = ui, server = server)
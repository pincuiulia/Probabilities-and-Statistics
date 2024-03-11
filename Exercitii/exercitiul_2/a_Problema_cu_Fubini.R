library(shiny)
library(pracma)

# Definim UI pentru aplicatie
ui <- fluidPage(
  
  # Afiseaza un panou de titlu pentru aplicatie
  titlePanel("Calculator Integrale Duble - Fubini"),
  
  # Layout cu panou lateral si principal
  sidebarLayout(
    sidebarPanel(
      # Elementele de intrare pentru utilizator
      textInput("functie", "f(x, y):", value = "x * y"),
      numericInput("x_min", "Limita Inferioara pentru x:", value = 0),
      numericInput("x_max", "Limita Superioara pentru x:", value = 1),
      numericInput("y_min", "Limita Inferioara pentru y:", value = 0),
      numericInput("y_max", "Limita Superioara pentru y:", value = 1),
      # Buton pentru declansarea calculului si afisarea rezultatului
      actionButton("verifica", "Verifica")
    ),
    mainPanel(
      # Ceea ce se afiseaza
      textOutput("rezultat_iterativ"),
      textOutput("rezultat_permutat")
    )
  )
)

# Definirea server-ului logic
server <- function(input, output) {
  
  # Iterativa - integrala interioara mai intai
  output$rezultat_iterativ <- renderText({
    # Conditia asigura faptul ca se efectueaza calculul doar atunci
    # cand butonul "Verifica" e apasat
    if (input$verifica > 0) {
      # Definirea si evaluarea functiei
      f <- function(x, y) {
        eval(parse(text = input$functie), list(x = x, y = y))
      }
      
      # Generare de variabile aleatoare bidimensionale
      nr_observatii <- 1000
      x <- runif(nr_observatii, min = input$x_min, max = input$x_max)
      y <- runif(nr_observatii, min = input$y_min, max = input$y_max)
      
      # Calculul integralelor folosind metoda iterativa
      x_values <- seq(input$x_min, input$x_max, length.out = 100)
      y_values <- seq(input$y_min, input$y_max, length.out = 100)
      z_values <- outer(x_values, y_values, Vectorize(function(x, y) f(x, y)))
      integrala_iterativa <- sum(z_values) * ((input$x_max - input$x_min) / length(x_values)) * ((input$y_max - input$y_min) / length(y_values))
      
      paste("Rezultat Iterativ: ", round(integrala_iterativa, 4))
    }
  })
  
  # Permutata - ordinea de integrare este inversata
  output$rezultat_permutat <- renderText({
    if (input$verifica > 0) {
      # Definirea si evaluarea functiei
      f <- function(x, y) {
        eval(parse(text = input$functie), list(x = x, y = y))
      }
      
      # Generare de variabile aleatoare bidimensionale
      nr_observatii <- 1000
      x <- runif(nr_observatii, min = input$x_min, max = input$x_max)
      y <- runif(nr_observatii, min = input$y_min, max = input$y_max)
      
      # Calculul integralelor folosind metoda permutata
      x_values <- seq(input$x_min, input$x_max, length.out = 100)
      y_values <- seq(input$y_min, input$y_max, length.out = 100)
      z_permutat <- outer(y_values, x_values, Vectorize(function(y, x) f(x, y)))
      integrala_permutata <- sum(z_permutat) * ((input$y_max - input$y_min) / length(y_values)) * ((input$x_max - input$x_min) / length(x_values))
      
      paste("Rezultat Permutat: ", round(integrala_permutata, 4))
    }
  })
}

# Rularea aplicatiei Shiny
shinyApp(ui = ui, server = server)

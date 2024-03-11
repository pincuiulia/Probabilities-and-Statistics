library(shiny)
library(animate)

# Definirea funnctiei pentru verificarea densitatii
# de probabilitate
verificare_densitate <- function(f, x, y) {
  z <- try(f(x, y), silent = TRUE)
  
  # Verificarea daca rezultatul este o densitate de probabilitate
  if (!inherits(z, "try-error") && all(z >= 0) && all(z <= 1) && all(is.finite(z))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Definire UI
ui <- fluidPage(
  
  titlePanel("Verificare Densitate de Probabilitate"),
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
      verbatimTextOutput("rezultat")
    )
  )
)

# Definire server logic
server <- function(input, output) {
  # Se activeaza verificarea la apasarea butonului
  observeEvent(input$verifica, {
    # Definirea si verificarea functiei
    f <- function(x, y) {
      eval(parse(text = input$functie), list(x = x, y = y))
    }
    
    # Generare de variabile aleatoare bidimensionale
    nr_observatii <- 1000
    x <- runif(nr_observatii, min = input$x_min, max = input$x_max)
    y <- runif(nr_observatii, min = input$y_min, max = input$y_max)
    
    # verificare f densitate de probabilitate
    if(verificare_densitate(f, x, y)) {
      output$rezultat <- renderText("Functia introdusa este o densitate de probabilitate.")
    } else {
      output$rezultat <- renderText("Functia introdusa NU este o densitate de probabilitate.")
    }
  })
}

# Rulare aplicatie
shinyApp(ui = ui, server = server)
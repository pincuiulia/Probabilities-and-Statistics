library(shiny)

# Functie pentru calculul densitatii marginale a lui X
dens_marg_x <- function(densitate, x) {
  integrate(function(y) densitate(x, y), lower = -Inf, upper = Inf)$value
}

# Functie pentru calculul densitatii marginale a lui Y
dens_marg_y <- function(densitate, y) {
  integrate(function(x) densitate(x, y), lower = -Inf, upper = Inf)$value
}

# Functie pentru calculul densitatii conditionate f(x|Y=y)
dens_cond_x <- function(densitate, x, y) {
  densitate(x, y) / dens_marg_y(densitate, y)
}

# Functie pentru calculul densitatii conditionate f(Y|X=x)
dens_cond_y <- function(densitate, x, y) {
  densitate(x, y) / dens_marg_x(densitate, x)
}

# UI
ui <- fluidPage(
  titlePanel("Densitati marginale si conditionate"),
  sidebarLayout(
    sidebarPanel(
      textInput("densityFunction", "Introduceti densitatea comuna f(x, y):", "dnorm(x)*dnorm(y)"),
      numericInput("x_value", "Introduceti valoarea pentru x:", value = 0),
      numericInput("y_value", "Introduceti valoarea pentru y:", value = 0),
      actionButton("goButton", "Calculeaza"),
      br(),
      helpText("Introduceti o expresie matematica care sa reprezinte densitatea comuna a X și Y, folosind x și y ca variabile.")
    ),
    mainPanel(
      textOutput("marginalX"),
      textOutput("marginalY"),
      textOutput("conditionalX"),
      textOutput("conditionalY")
    )
  )
)

# Server
server <- function(input, output) {
  
  observeEvent(input$goButton, {
    req(input$densityFunction)
    
    # Definirea densității comună f(x, y)
    densitate <- function(x, y) {
      eval(parse(text = input$densityFunction))
    }
    
    x <- input$x_value
    y <- input$y_value
    
    # Calcul densitatea marginala a lui X
    marginal_x <- dens_marg_x(densitate, x)
    
    # Calcul densitatea marginala a lui Y
    marginal_y <- dens_marg_y(densitate, y)
    
    # Calcul densitatea conditionata f(x|Y=y)
    conditional_x <- dens_cond_x(densitate, x, y)
    
    # Calcul densitatea conditionata f(Y|X=x)
    conditional_y <- dens_cond_y(densitate, x, y)
    
    output$marginalX <- renderText({
      paste("Densitatea marginala a lui X:", marginal_x)
    })
    
    output$marginalY <- renderText({
      paste("Densitatea marginala a lui Y:", marginal_y)
    })
    
    output$conditionalX <- renderText({
      paste("Densitatea conditionata f(x | Y = y):", conditional_x)
    })
    
    output$conditionalY <- renderText({
      paste("Densitatea conditionata f(Y | X = x):", conditional_y)
    })
  })
}

shinyApp(ui, server)
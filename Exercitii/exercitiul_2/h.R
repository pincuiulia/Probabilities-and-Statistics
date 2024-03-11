library(shiny) 

#crearea panoului pentru introducerea datelor 
ui <- fluidPage( 
  titlePanel("Calculator pentru media si dispersia lui g(X)"), 
  sidebarLayout( 
    sidebarPanel( 
      textInput("g_function", "Introduceti functia g(x):", "x^2"), 
      actionButton("calculate", "Calculeaza"), 
      tags$hr(), 
      h4("Rezultate:"), 
      verbatimTextOutput("medie_gX_output"), 
      verbatimTextOutput("var_gX_output") 
    ), 
    mainPanel() 
  ) 
) 

# Server pentru a calcula 
server <- function(input, output) { 
  calc_medie_si_var <- function(g_function) { 
    medie_gX <- integrate(function(x) eval(parse(text = g_function)) * dnorm(x), lower = -Inf, upper = Inf)$value 
    #calculam media si media patratului 
    medie_gX_sq <- integrate(function(x) (eval(parse(text = g_function)))^2 * dnorm(x), lower = -Inf, upper = Inf)$value 
    
    var_gX <- medie_gX_sq - medie_gX^2 
    
    return(list(medie_gX = medie_gX, var_gX = var_gX)) 
  } 
  #initiaza calculul si afisarea rezultatului 
  observeEvent(input$calculate, { 
    results <- calc_medie_si_var(input$g_function) 
    
    output$medie_gX_output <- renderPrint(paste("Media lui g(X):", results$medie_gX)) 
    output$var_gX_output <- renderPrint(paste("Dispersia lui g(X):", results$var_gX)) 
  }) 
} 
shinyApp(ui, server) 

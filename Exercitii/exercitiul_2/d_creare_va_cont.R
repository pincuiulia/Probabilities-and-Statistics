library(shiny)
library(stats)
library(MASS)

# Definire UI
ui <- fluidPage(
  titlePanel("Creare Variabila Aleatoare Continua"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("func_input", "Introduceti functia de densitate:", "exp(-(x^2 + y^2)/2)/(2*pi)"),
      radioButtons("variabile", "Selectati variabilele:", choices = c("Unidimensionala", "Bidimensionala")),
      actionButton("creare_variabila_btn", "Creeaza Variabila Aleatoare")
    ),
    mainPanel(
      # Rezultatul este afisat in panoul principal
      verbatimTextOutput("rezultat_text")
    )
  )
)

# Define server
server <- function(input, output) {
  # Activat cand utilizatorul apasa butonul
  observeEvent(input$creare_variabila_btn, {
    tryCatch({
      # Verifica dacă funcția de densitate introdusă de utilizator este validă
      f <- function(x, y) eval(parse(text = input$func_input))
      if (is.function(f)) {
        # Se verifica daca acesta este obtiunea aleasa de user
        if ("Unidimensionala" %in% input$variabile) {
          # Crează variabilă aleatoare unidimensională utilizand rnorm()
          univar <- Vectorize(function(n) rnorm(n, mean = 0, sd = 1))
          output$rezultat_text <- renderPrint({
            univar(5)  # Afișează primele 5 observații
          })
        }
        # Se verifica daca aceasta este obtiunea aleasa de user
        if ("Bidimensionala" %in% input$variabile) {
          # Crează variabilă aleatoare bidimensională utilizand mvnorm()
          bivar <- Vectorize(function(n) mvrnorm(n, mu = c(0, 0), Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2)))
          output$rezultat_text <- renderPrint({
            bivar(5)  # Afișează primele 5 observații
          })
        }
      } else {
        # Cand utilizatorul apasa butonul, codul incepe sa verifice
        # daca functia de densitate introdusa este valida
        # Asta este eroarea pentru cazul contrar
        output$rezultat_text <- renderText("Eroare: Functia de densitate introdusa nu este valida.")
      }
    }, error = function(e) {
      output$rezultat_text <- renderText("Eroare: Functia introdusa nu este valida.")
    })
  })
}

shinyApp(ui, server)
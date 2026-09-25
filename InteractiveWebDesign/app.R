library(shiny)
library(readr)

compress_image <- function(X, k) {
  V <- t(X) %*% X
  eig <- eigen(V)
  U <- eig$vectors
  Z <- X %*% U
  X_k <- Z[, 1:k, drop = FALSE] %*% t(U[, 1:k, drop = FALSE])
  err <- norm(X - X_k, type = "F")
  list(approx = X_k, error = err)
}

show_image <- function(M, title = "") {
  image(t(apply(M, 2, rev)), col = gray.colors(256), axes = FALSE, main = title)
}

ui <- fluidPage(
  titlePanel("PCA Image Compressor"),
  
  fluidRow(
    column(
      width = 4,
      fileInput("file", "Upload Image (.csv)", accept = ".csv"),
      uiOutput("kSliderUI"),
      textOutput("errorText")
    ),
    column(
      width = 8,
      fluidRow(
        column(6, plotOutput("originalPlot")),
        column(6, plotOutput("compressedPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  img_matrix <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    data.matrix(df)
  })
  
  output$kSliderUI <- renderUI({
    req(img_matrix())
    p <- ncol(img_matrix())
    sliderInput("k", "Compression level (k)", min = 1, max = p, value = min(10, p), step = 1)
  })
  
  compressed <- reactive({
    req(img_matrix(), input$k)
    compress_image(img_matrix(), input$k)
  })
  
  output$originalPlot <- renderPlot({
    req(img_matrix())
    show_image(img_matrix(), "Original")
  })
  
  output$compressedPlot <- renderPlot({
    req(compressed())
    show_image(compressed()$approx, paste0("Compressed (k = ", input$k, ")"))
  })
  
  output$errorText <- renderText({
    req(compressed())
    paste0("Frobenius norm error: ", round(compressed()$error, 3))
  })
}

shinyApp(ui, server)
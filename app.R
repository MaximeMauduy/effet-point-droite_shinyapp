# app_sensibilisation.R — Sensibilisation à l'impact d'un point sur la régression
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("📈 Impact d’un point sur la droite de régression"),
  p("Cher.e étudiant.e, ajoute un point (rouge) et observe comment il influence la droite de régression."),
  p("La droite bleue correspond au modèle initial (sans le point rouge)."),
  p("La droite rouge pointillée correspond au modèle avec le point rouge ajouté."),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("newdata", "🎲 Nouveau jeu de données"),
      tags$hr(),
      h4("Coordonnées du point rouge"),
      uiOutput("xSlider"),
      uiOutput("ySlider"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(7, plotOutput("scatterPlot", height = 420)),
        column(5,
               h4("Équations des droites"),
               textOutput("eq_bleu"),
               textOutput("eq_rouge")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # générateur jeu de données
  gen_dataset <- function(seed = NULL){
    if (!is.null(seed)) set.seed(seed)
    n <- sample(10:14, 1)
    x <- sort(runif(n, 0, 10))
    b0 <- runif(1, 1, 5)
    b1 <- sample(c(-1.2, -0.8, -0.4, 0, 0.3, 0.7, 1.2, 1.8), 1)
    sigma <- sample(c(0.4, 0.8, 1.2, 2, 3), 1)
    y <- b0 + b1 * x + rnorm(n, 0, sigma)
    data.frame(x = x, y = y)
  }
  
  base_data <- reactiveVal(gen_dataset(123))
  
  observeEvent(input$newdata, {
    seed <- as.integer(Sys.time()) %% 10000
    base_data(gen_dataset(seed))
  })
  
  # sliders dynamiques
  output$xSlider <- renderUI({
    df <- base_data(); req(df)
    xr <- range(df$x); margin <- max(0.5, diff(xr) * 0.25)
    sliderInput("x_new", "X du point rouge :", min = floor(xr[1] - margin),
                max = ceiling(xr[2] + margin), value = round(mean(xr), 1), step = 0.1)
  })
  output$ySlider <- renderUI({
    df <- base_data(); req(df)
    yr <- range(df$y); margin <- max(0.5, diff(yr) * 0.25)
    sliderInput("y_new", "Y du point rouge :", min = floor(yr[1] - margin),
                max = ceiling(yr[2] + margin), value = round(mean(yr), 1), step = 0.1)
  })
  
  # données complètes
  data_all <- reactive({
    df <- base_data(); req(df)
    if (is.null(input$x_new) || is.null(input$y_new)) return(df)
    df2 <- rbind(df, data.frame(x = input$x_new, y = input$y_new))
    df2$id <- seq_len(nrow(df2))
    df2$nouveau_point <- ifelse(seq_len(nrow(df2)) == nrow(df2), "OUI", "NON")
    df2
  })
  
  # modèles de régression
  model_base <- reactive({
    df <- base_data(); req(df)
    lm(y ~ x, data = df)
  })
  
  model_all <- reactive({
    df <- data_all(); req(df)
    lm(y ~ x, data = df)
  })
  
  # graphique
  output$scatterPlot <- renderPlot({
    df <- data_all(); req(df)
    m_base <- model_base(); m_all <- model_all()
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(color = nouveau_point), size = 3) +
      scale_color_manual(values = c("NON"="black", "OUI"="red"), guide=FALSE) +
      theme_minimal()
    
    # droite bleue (base)
    if (!is.null(m_base)) {
      p <- p + geom_smooth(data = base_data(), aes(x = x, y = y),
                           method = "lm", se = FALSE, color = "blue")
    }
    
    # droite rouge (avec le point)
    if (!is.null(m_all)) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")
    }
    
    p
  })
  
  # équations
  output$eq_bleu <- renderText({
    m <- model_base(); req(m)
    coefs <- coef(m)
    paste0("Droite bleue (sans point rouge) : y = ",
           round(coefs[1], 2), " + ", round(coefs[2], 2), "x")
  })
  
  output$eq_rouge <- renderText({
    m <- model_all(); req(m)
    coefs <- coef(m)
    paste0("Droite rouge (avec point rouge) : y = ",
           round(coefs[1], 2), " + ", round(coefs[2], 2), "x")
  })
  
}

shinyApp(ui, server)

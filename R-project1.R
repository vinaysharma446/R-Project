library(MASS)
library(shiny)
library(shinythemes)
library(ggplot2)

data("Boston")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  titlePanel("Vinay's Enhanced Model Summary and Residuals Plot"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Selection and Parameters"),
      radioButtons("model_type", "Choose Model Type:",
                   choices = list("Linear Model" = "linear", 
                                  "Polynomial Model" = "poly")),
      conditionalPanel(
        condition = "input.model_type == 'poly'",
        sliderInput("poly_degree", "Select Polynomial Degree:",
                    min = 2, max = 5, value = 2)
      ),
      actionButton("update", "Update Model", class = "btn-primary"),
      hr(),
      h5("Additional Features"),
      checkboxInput("show_data", "Show Dataset Summary", FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Model Summary", 
                 verbatimTextOutput("model_summary")),
        tabPanel("Residuals Plot", 
                 plotOutput("residuals_plot")),
        tabPanel("R-squared Values", 
                 verbatimTextOutput("r_squared")),
        tabPanel("Dataset Summary", 
                 conditionalPanel(
                   condition = "input.show_data == true",
                   tableOutput("data_summary")
                 ))
      )
    )
  )
)

server <- function(input, output) {
  fit_model <- reactive({
    if (input$model_type == "linear") {
      lm(medv ~ ., data = Boston)
    } else {
      lm(medv ~ poly(rm, input$poly_degree) + ., data = Boston)
    }
  })
  
  output$model_summary <- renderPrint({
    input$update
    isolate(summary(fit_model()))
  })
  
  output$residuals_plot <- renderPlot({
    input$update
    isolate({
      model <- fit_model()
      residuals <- model$residuals
      residuals_data <- data.frame(Index = 1:length(residuals), Residuals = residuals)
      
      ggplot(residuals_data, aes(x = Index, y = Residuals)) +
        geom_point(aes(color = Residuals > 0), size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        scale_color_manual(values = c("green", "blue")) +
        labs(title = "Residuals Plot: Observed vs. Predicted Values",
             y = "Residuals", x = "Index") +
        theme_minimal()
    })
  })
  
  output$r_squared <- renderPrint({
    input$update
    isolate({
      r_squared <- summary(fit_model())$r.squared
      paste("R-squared:", round(r_squared, 4))
    })
  })
  
  output$data_summary <- renderTable({
    input$update
    isolate({
      summary(Boston)
    })
  })
}

shinyApp(ui = ui, server = server)
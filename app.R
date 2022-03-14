library(shiny)
library(dplyr)

source("MRC_all_shiny.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Multiple Regression Power"),
  
  # 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "predictors",
                  label   = "Number of Predictors: ",
                  min     = 1,
                  max     = 5,
                  value   = 1,
                  step    = 1),
      numericInput(inputId = "n",
                   label   = "Sample Size:",
                   min     = 1,
                   max     = Inf,
                   value   = 100,
                   step    = 1),
      numericInput(inputId = "alpha",
                   label   = "Alpha: ",
                   min     = .001,
                   max     = 1.0,
                   value   = 0.05),
      numericInput(inputId = "nruns",
                   label   = "Number of Iterations",
                   min     = 1,
                   max     = 10000,
                   value   = 10000),
      numericInput(inputId  = "ry1",
                   label   = "Correlation between Y and 1st Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "ry2",
                   label   = "Correlation between Y and 2nd Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "ry3",
                   label   = "Correlation between Y and 3rd Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "ry4",
                   label   = "Correlation between Y and 4th Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "ry5",
                   label   = "Correlation between Y and 5th Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r12",
                   label   = "Correlation between 1st and 2nd Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r13",
                   label   = "Correlation between 1st and 3rd Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r14",
                   label   = "Correlation between 1st and 4th Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r15",
                   label   = "Correlation between 1st and 5th Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r23",
                   label   = "Correlation between 2nd and 3rd Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r24",
                   label   = "Correlation between 2nd and 4th Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r25",
                   label   = "Correlation between 2nd and 5th Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r34",
                   label   = "Correlation between 3rd and 4th Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r35",
                   label   = "Correlation between 3rd and 5th Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      numericInput(inputId  = "r45",
                   label   = "Correlation between 4th and 5th Predictor",
                   min     = 0,
                   max     = 1,
                   value   = 0.5,
                   step    = 0.01),
      
      actionButton(inputId = "run_function",
                   label   = "Calculate"),
      
      # SET WIDTH FOR SIDEBAR PANEL
      width = 7
    ),
    
    mainPanel(
      verbatimTextOutput(outputId = "power_results"),
      
      # SET WIDTH FOR MAIN PANEL
      width = 5
    )
  )
)

# Define server logic to print output returned by MRC_all()
server <- function(input, output) {
  
  power_vals <- reactive({
    MRC_all(ry1 = input$ry1, ry2 = input$ry2, ry3 = input$ry3, r12 = input$r12, r13 = input$r13, r23 = input$r23,
            ry14 = input$ry14, ry15 = input$ry15, ry24 = input$ry24, ry25 = input$ry25, ry34 = input$ry34, ry35 = input$ry35,  
            ry45 = input$ry45, n = input$n, alpha = input$alpha, nruns = input$nruns, predictors = input$predictors)
  })
  
  output$power_results <- eventReactive(input$run_function, {
    
    
    
    # 1 predictor
    if (input$predictors == 1){
      paste0(power_vals()$n_print, "\n",
             power_vals()$power_r2_print, "\n",
             power_vals()$power_b1_print, "\n", 
             power_vals()$power_none_print, "\n", 
             power_vals()$power_one_print, "\n")
    } else if (input$predictors == 2){
      paste0(power_vals()$n_print, "\n",
             power_vals()$power_r2_print, "\n",
             power_vals()$power_b1_print, "\n",
             power_vals()$power_b2_print, "\n", 
             power_vals()$power_none_print, "\n", 
             power_vals()$power_one_print, "\n", 
             power_vals()$power_all_print)
    } else if (input$predictors == 3){
      paste0(power_vals()$n_print, "\n",
             power_vals()$power_r2_print, "\n",
             power_vals()$power_b1_print, "\n",
             power_vals()$power_b2_print, "\n", 
             power_vals()$power_b3_print, "\n", 
             power_vals()$power_none_print, "\n", 
             power_vals()$power_one_print, "\n", 
             power_vals()$power_two_print, "\n", 
             power_vals()$power_all_print)
    
  } else if (input$predictors == 4){
    paste0(power_vals()$n_print, "\n",
           power_vals()$power_r2_print, "\n",
           power_vals()$power_b1_print, "\n",
           power_vals()$power_b2_print, "\n", 
           power_vals()$power_b3_print, "\n", 
           power_vals()$power_b4_print, "\n", 
           power_vals()$power_none_print, "\n", 
           power_vals()$power_one_print, "\n", 
           power_vals()$power_two_print, "\n", 
           power_vals()$power_three_print, "\n", 
           power_vals()$power_all_print)
  } else if (input$predictors == 5){
    paste0(power_vals()$n_print, "\n",
           power_vals()$power_r2_print, "\n",
           power_vals()$power_b1_print, "\n",
           power_vals()$power_b2_print, "\n", 
           power_vals()$power_b3_print, "\n", 
           power_vals()$power_b4_print, "\n", 
           power_vals()$power_b5_print, "\n", 
           power_vals()$power_none_print, "\n", 
           power_vals()$power_one_print, "\n", 
           power_vals()$power_two_print, "\n", 
           power_vals()$power_three_print, "\n", 
           power_vals()$power_four_print, "\n", 
           power_vals()$power_all_print)
    
  }
  
})}

# Run the application 
shinyApp(ui = ui, server = server)

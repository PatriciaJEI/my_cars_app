library(shiny)
library(datasets)
data("mtcars")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # My title
    titlePanel("Playing with the mtcars dataset"),
    
    sidebarLayout(
        # Generating the left panel with the input data 
        # and checkboxes to generate the model
        sidebarPanel(
            
            # Inputting variables to use
            h3("What parameters do you want in your model?"),
            checkboxGroupInput("predictors", "Choose from de list:", 
                               choices = c("Cylinders"="cyl", "Displacement"="disp", 
                                           "Horsepower"="hp", "Weight"="wt", 
                                           "Quarter mile time"="qsec", 
                                           "Transmission"="am"),
                               selected = c("Horsepower"="hp")),
            
            # Inputting values for the prediction
            h3("Pick some values to predict with"),
            sliderInput("slider_cyl", "Fix the cylinders", min = 4, max = 8, 
                        value = 6, step = 2),
            sliderInput("slider_disp", "Fix the displacement", min = 75, 
                        max = 470, value = 200, step = 5),
            sliderInput("slider_hp", "Fix the horsepower", min = 55, max = 335, 
                        value = 150, step = 5),
            sliderInput("slider_wt", "Fix the weight", min = 1.5, max = 5.5, 
                        value = 3.5, step = 0.25),
            sliderInput("slider_qsec", "Fix the quarter mile time", min = 14.5, 
                        max = 23, value = 18.5, step = 0.5),
            sliderInput("slider_am", "Fix the transmission (0=automatic, 1=manual)", 
                        min = 0, max = 1, value = 1, step = 1),
            submitButton("Predict with your data")
        ),

        # Showing six plots and the prediction
        mainPanel(
            h3("Here is your fitted graph"),
            tabsetPanel(type = "tabs",
                        tabPanel("Cylinders", br(), plotOutput("Plot1")),
                        
                        tabPanel("Displacement", br(), plotOutput("Plot2")),
                        
                        tabPanel("Horsepower", br(), plotOutput("Plot3")),
                        
                        tabPanel("Weight", br(), plotOutput("Plot4")),
                        
                        tabPanel("Quarter mile time", br(), plotOutput("Plot5")),
                        
                        tabPanel("Transmission", br(), plotOutput("Plot6"))
                        ),
            h3("Predicted Miles per Gallon from Model:"),
            textOutput("pred")
            
        )
    )
))

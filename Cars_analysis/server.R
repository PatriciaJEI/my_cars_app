library(shiny)
library(datasets)
data("mtcars")

shinyServer(function(input, output) {
    
    # Producing the linear model
    
    model <- reactive({
        lm(as.formula(paste("mpg ~ ", paste0(input$predictors, collapse="+"))), 
           data=mtcars)
    })
    
    intercepts <- reactive({
        int <- numeric(0)
        if("cyl" %in% input$predictors){
            int <- c(int, model()$coefficients["cyl"])
        }else{
                int <- c(int, 0)
            }
        if("disp" %in% input$predictors){
            int <- c(int, model()$coefficients["disp"])
        }else{
            int <- c(int, 0)
        }
        if("hp" %in% input$predictors){
            int <- c(int, model()$coefficients["hp"])
        }else{
            int <- c(int, 0)
        }
        if("wt" %in% input$predictors){
            int <- c(int, model()$coefficients["wt"])
        }else{
            int <- c(int, 0)
        }
        if("qsec" %in% input$predictors){
            int <- c(int, model()$coefficients["qsec"])
        }else{
            int <- c(int, 0)
        }
        if("am" %in% input$predictors){
            int <- c(int, model()$coefficients["am"])
        }else{
            int <- c(int, 0)
        }
        int
    })
    # Predicting with the input values
    
    modelPred <- reactive({
        cylIn <- input$slider_cyl; dispIn <- input$slider_disp
        hpIn <- input$slider_hp; wtIn <- input$slider_wt 
        qsecIn <- input$slider_qsec; amIn <- input$slider_am
        
        predict(model(), newdata = data.frame(cyl=cylIn, disp=dispIn, hp=hpIn,
                                              wt=wtIn, qsec=qsecIn, am=amIn))
    })
    
    # Producing the 6 graphics: mpg vs. variable, for the six selected variables
    
    # Cylinders
    output$Plot1 <- renderPlot({
        cylIn <- input$slider_cyl; dispIn <- input$slider_disp
        hpIn <- input$slider_hp; wtIn <- input$slider_wt 
        qsecIn <- input$slider_qsec; amIn <- input$slider_am
        
        plot(mtcars$cyl, mtcars$mpg, xlab = "Cylinders", ylab = "Miles per gallon", 
             bty = "n", pch = 16, ylim = c(10,35), xlim = c(3, 9))
        if("cyl" %in% input$predictors){
        abline(a=model()$coefficients["(Intercept)"]
               +intercepts()[2]*dispIn
               +intercepts()[3]*hpIn
               +intercepts()[4]*wtIn
               +intercepts()[5]*qsecIn
               +intercepts()[6]*amIn, 
               b=model()$coefficients["cyl"],
               col = "red", lwd = 2)
        points(cylIn, modelPred(), col = "red", pch = 16, cex = 2)}
    })
    
    # Displacement
    output$Plot2 <- renderPlot({
        cylIn <- input$slider_cyl; dispIn <- input$slider_disp
        hpIn <- input$slider_hp; wtIn <- input$slider_wt 
        qsecIn <- input$slider_qsec; amIn <- input$slider_am
        
        plot(mtcars$disp, mtcars$mpg, xlab = "Displacement", ylab = "Miles per gallon", 
             bty = "n", pch = 16, ylim = c(10,35), xlim = c(70, 475))
        if("disp" %in% input$predictors){
        abline(a=model()$coefficients["(Intercept)"]
               +intercepts()[1]*cylIn
               +intercepts()[3]*hpIn
               +intercepts()[4]*wtIn
               +intercepts()[5]*qsecIn
               +intercepts()[6]*amIn, 
               b=model()$coefficients["disp"],
               col = "red", lwd = 2)
        points(dispIn, modelPred(), col = "red", pch = 16, cex = 2)}
    })
    
    # Horsepower
    output$Plot3 <- renderPlot({
        cylIn <- input$slider_cyl; dispIn <- input$slider_disp
        hpIn <- input$slider_hp; wtIn <- input$slider_wt 
        qsecIn <- input$slider_qsec; amIn <- input$slider_am
        
        plot(mtcars$hp, mtcars$mpg, xlab = "Horsepower", ylab = "Miles per gallon", 
             bty = "n", pch = 16, ylim = c(10,35), xlim = c(50, 350))
        if("hp" %in% input$predictors){
            abline(a=model()$coefficients["(Intercept)"]
                   +intercepts()[1]*cylIn
                   +intercepts()[2]*dispIn
                   +intercepts()[4]*wtIn
                   +intercepts()[5]*qsecIn
                   +intercepts()[6]*amIn, 
                   b=model()$coefficients["hp"],
                   col = "red", lwd = 2)
            points(hpIn, modelPred(), col = "red", pch = 16, cex = 2)}
    })
    
    # Weight
    output$Plot4 <- renderPlot({
        cylIn <- input$slider_cyl; dispIn <- input$slider_disp
        hpIn <- input$slider_hp; wtIn <- input$slider_wt 
        qsecIn <- input$slider_qsec; amIn <- input$slider_am
        
        plot(mtcars$wt, mtcars$mpg, xlab = "Weight", ylab = "Miles per gallon", 
             bty = "n", pch = 16, ylim = c(10,35), xlim = c(1.5, 5.5))
        if("wt" %in% input$predictors){
        abline(a=model()$coefficients["(Intercept)"]
               +intercepts()[1]*cylIn
               +intercepts()[2]*dispIn
               +intercepts()[3]*hpIn
               +intercepts()[5]*qsecIn
               +intercepts()[6]*amIn, 
               b=model()$coefficients["wt"],
               col = "red", lwd = 2)
        points(wtIn, modelPred(), col = "red", pch = 16, cex = 2)}
    })
    
    # Quarter mile time
    output$Plot5 <- renderPlot({
        cylIn <- input$slider_cyl; dispIn <- input$slider_disp
        hpIn <- input$slider_hp; wtIn <- input$slider_wt 
        qsecIn <- input$slider_qsec; amIn <- input$slider_am
        
        plot(mtcars$qsec, mtcars$mpg, xlab = "Quarter mile time", 
             ylab = "Miles per gallon", 
             bty = "n", pch = 16, ylim = c(10,35), xlim = c(14, 23))
        if("qsec" %in% input$predictors){
        abline(a=model()$coefficients["(Intercept)"]
               +intercepts()[1]*cylIn
               +intercepts()[2]*dispIn
               +intercepts()[3]*hpIn
               +intercepts()[4]*wtIn
               +intercepts()[6]*amIn, 
               b=model()$coefficients["qsec"],
               col = "red", lwd = 2)
        points(qsecIn, modelPred(), col = "red", pch = 16, cex = 2)}
    })
    
    # Transmission type
    output$Plot6 <- renderPlot({
        cylIn <- input$slider_cyl; dispIn <- input$slider_disp
        hpIn <- input$slider_hp; wtIn <- input$slider_wt 
        qsecIn <- input$slider_qsec; amIn <- input$slider_am
        
        plot(mtcars$am, mtcars$mpg, xlab = "Transmission", ylab = "Miles per gallon", 
             bty = "n", pch = 16, ylim = c(10,35), xlim = c(-0.5, 1.5))
        if("am" %in% input$predictors){
        abline(a=model()$coefficients["(Intercept)"]
               +intercepts()[1]*cylIn
               +intercepts()[2]*dispIn
               +intercepts()[3]*hpIn
               +intercepts()[4]*wtIn
               +intercepts()[5]*qsecIn, 
               b=model()$coefficients["am"],
               col = "red", lwd = 2)
        points(amIn, modelPred(), col = "red", pch = 16, cex = 2)}
    })
    
    # Prediction message
    output$pred <- renderText({
        paste(round(modelPred(),3), "Miles per Gallon", sep =" ")
    })

})

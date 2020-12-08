# my_cars_app
Repository for the mtcars application

- The folder cars_app contains the two files ui.R and server.R to run the app
- The folder presentation contains the files that generate the pitch presentation

My app takes the mtcars dataset from R and perform a series of linear models, depending on which variables you want to include as predictors, to obtain miles per gallon as outcom. There are sliders (on for each available variable to include in the model), to provide input data in order to predict with. There are also six tabs, on for each plot with y = miles per gallon and x = other variable, with their individual regression line if the "other variable" wa included in the model, and the predicted point.

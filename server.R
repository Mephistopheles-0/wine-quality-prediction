
library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggfortify)

###### set up shiny server

shinyServer(function(input, output, session) {
  
  ########### explore tab
  
  data_to_explore <- reactive({
    fread('./data/prepared_data/wine_data.csv') %>%
      filter(type == input$wine_type_explore) %>%
      select(quality, input$variable_to_explore)
  })
  
  outliers <- reactive({
    data_to_find_outliers <- data_to_explore()
    data_to_find_outliers <- data_to_find_outliers[, input$variable_to_explore]
    boxplot.stats(data_to_find_outliers)$out
  })
  
  data_to_explore_cleaned <- reactive({
    if (input$exclude_outliers_explore == 'no') {
      data_to_explore()
    } else if (input$exclude_outliers_explore == 'yes') {
      outliers_to_exclude <- outliers()
      data_without_outliers <- data_to_explore()
      variable_without_outliers <- data_without_outliers[, input$variable_to_explore]
      variable_without_outliers[variable_without_outliers %in% outliers_to_exclude] <- NA
      data_without_outliers[, input$variable_to_explore] <- variable_without_outliers
      data_without_outliers
    }
  })
  
  color_explore <- reactive({
    if (input$wine_type_explore == 'red') {
      "#864040"
    } else if (input$wine_type_explore == 'white') {
      "#BEDE7E"
    }
  })
  
  output$variable_summary <- renderTable({
    data_to_summarize <- data_to_explore_cleaned() %>%
      select(-quality)
    data_to_summarize <- as.vector(data_to_summarize[,input$variable_to_explore])
    if (input$exclude_outliers_explore == 'no') {
      n_outliers <- length(outliers())
    } else if (input$exclude_outliers_explore == 'yes') {
      n_outliers <- 0
    }
    summary_table <- data.frame(cbind(
      c('Minimum', 'Maximum', 'Mean', 'Median', 'IQR', 'N Outliers'),
                        c(min(data_to_summarize, na.rm = TRUE),
                          max(data_to_summarize, na.rm = TRUE),
                          mean(data_to_summarize, na.rm = TRUE),
                          median(data_to_summarize, na.rm = TRUE),
                          IQR(data_to_summarize, na.rm = TRUE),
                          n_outliers)))
    colnames(summary_table) <- c('Summary', '')
    summary_table
  })
  
  output$variable_histogram <- renderPlot({
    data_to_plot <- data_to_explore_cleaned() %>%
      select(-quality)
    hist(as.vector(data_to_plot[,input$variable_to_explore]), 
         main = "Distribution: ", 
         xlab = input$variable_to_explore, ylab = "frequency",
         col = color_explore())
  })
  
  output$variable_qualitycor <- renderPlot({
    data_to_plot_2 <- data_to_explore()
    data_to_plot_2_noOutliers <- data_to_explore_cleaned()
    plot(data_to_plot_2_noOutliers[, input$variable_to_explore], data_to_plot_2_noOutliers[, "quality"],
         main = "Relationship with quality:",
         xlab = input$variable_to_explore, ylab = "wine quality",
         col = color_explore(),
         abline(lm(data_to_plot_2_noOutliers$quality ~ data_to_plot_2_noOutliers[, input$variable_to_explore]), lwd = 3))
  })
  
  output$variable_boxplot <- renderPlot({
    data_to_plot_boxplot <- data_to_explore_cleaned()
    boxplot(data_to_plot_boxplot[,input$variable_to_explore], 
            main = "Boxplot:", 
            xlab = input$variable_to_explore,
            col = color_explore())
  })
  
  ##### regression tab
  
  data_for_regression <- reactive({
    data_to_model <- fread('./data/prepared_data/wine_data.csv') %>%
      filter(type == input$wine_type_regression) %>%
      select(-type)
    vars_to_include <- append(input$variables_regression, 'quality')
    data_to_model[,vars_to_include]
  })
  
  data_for_regression_cleaned <- reactive({
    if (input$exclude_outliers_reg == 'no') {
      data_for_regression()
    } else if (input$exclude_outliers_reg == 'yes') {
      data_to_find_outliers_2 <- data_for_regression()
      data_to_find_outliers_2 <- data_for_regression()
      for (ii in 1:(ncol(data_to_find_outliers_2)-1)) {
        variable_without_outliers_2 <- data_to_find_outliers_2[, ii]
        outliers_to_exclude_2 <- boxplot.stats(variable_without_outliers_2)$out
        variable_without_outliers_2[variable_without_outliers_2 %in% outliers_to_exclude_2] <- NA
        data_to_find_outliers_2[, ii] <- variable_without_outliers_2
      }
      data_to_find_outliers_2
    }
    
  })
  
  color_reg <- reactive({
    if (input$wine_type_regression == 'red') {
      "#864040"
    } else if (input$wine_type_regression == 'white') {
      "#BEDE7E"
    }
  })
  
  model_reg_summary <- reactive({
    summary(lm(quality ~ ., data = data_for_regression_cleaned()))
  })
  
  model_reg <- reactive({
    lm(quality ~ ., data = data_for_regression_cleaned())
  })
  
  output$plot_regression <- renderPlot({
    par(mfrow = c(2,3))
    data_to_plot_3 <- data_for_regression_cleaned()
    for (ii in 1:(ncol(data_to_plot_3)-1)) {
      plot(data_to_plot_3[, ii], data_to_plot_3[, "quality"], 
           xlab = names(data_to_plot_3)[ii], ylab = "wine quality",
           col = color_reg(), 
           abline(model_reg(), lwd = 3))
    }
  })
  
  output$results_regression <- renderTable({
    results_table <- as.data.frame(model_reg_summary()$coefficients)
    results_table$Variable <- rownames(results_table)
    results_table %>%
      select(Variable, Estimate:`Pr(>|t|)`) %>%
      filter(Variable != '(Intercept)')
  })

  output$plot_regression <- renderPlot({
    autoplot(model_reg(), ncol = 2, label.size = 3, colour = color_reg()) + theme_bw()
  })
  
  output$rsquared_regression <- renderText({
    c('Model R-squared value: ', round(model_reg_summary()$r.squared, digits = 3))
  })
  
  ######### predict tab

  data_for_prediction <- reactive({
    data_to_model <- fread('./data/prepared_data/wine_data.csv') %>%
      filter(type == input$wine_type_regression) %>%
      select(-type)
  })
  
  data_for_prediction_cleaned <- reactive({
    if (input$exclude_outliers_pred == 'no') {
      data_for_prediction()
    } else if (input$exclude_outliers_pred == 'yes') {
      data_to_find_outliers_3 <- data_for_prediction()
      data_to_find_outliers_3 <- data_for_prediction()
      for (ii in 1:(ncol(data_to_find_outliers_3)-1)) {
        variable_without_outliers_3 <- data_to_find_outliers_3[, ii]
        outliers_to_exclude_3 <- boxplot.stats(variable_without_outliers_3)$out
        variable_without_outliers_3[variable_without_outliers_3 %in% outliers_to_exclude_3] <- NA
        data_to_find_outliers_3[, ii] <- variable_without_outliers_3
      }
      data_to_find_outliers_3
    }
    
  })
  
  color_pred <- reactive({
    if (input$wine_type_prediction == 'red') {
      "#864040"
    } else if (input$wine_type_prediction == 'white') {
      "#BEDE7E"
    }
  })
  
  model_pred <- reactive({
    lm(quality ~ acidity + alcohol + chlorides + density + sugar + sulfates,
       data = data_for_prediction_cleaned())
  })

  output$value_prediction <- renderText({
    
    input_acidity <- input$slider_acidity
    input_alcohol <- input$slider_alcohol
    input_chlorides <- input$slider_chlorides
    input_density <- input$slider_density
    input_sugar <- input$slider_sugar
    input_sulfates <- input$slider_sulfates
   c('A ', input$wine_type_prediction, ' vinho verde with the specified properties has a predicted quality of: ',
   round(as.vector(predict(model_pred(), newdata = data.frame(
      acidity = input_acidity, alcohol = input_alcohol,
      chlorides = input_chlorides, density = input_density,
      sugar = input_sugar, sulfates = input_sulfates))), digits = 1), '/ 10')
  })
  
  output$plot_prediction <- renderPlot({
    input_values <- c(input$slider_acidity, input$slider_alcohol, input$slider_chlorides,
                      input$slider_density, input$slider_sugar, input$slider_sulfates)
    predict_val_to_plot <- as.vector(predict(model_pred(), newdata = data.frame(
      acidity = input_values[1], alcohol = input_values[2],
      chlorides = input_values[3], density = input_values[4],
      sugar = input_values[5], sulfates = input_values[6])))
    
    par(mfrow = c(2,3))
    data_to_plot_4 <- data_for_prediction_cleaned() %>%
      select(-quality)
    data_to_plot_4_quality <- data_for_prediction_cleaned()$quality
    mean_values <- as.numeric(colMeans(data_to_plot_4))
    newData <- as.data.frame(matrix(rep(mean_values, nrow(data_to_plot_4)), 
                                    nrow = nrow(data_to_plot_4), ncol = length(mean_values),
                                    byrow = TRUE))
    names(newData) <- names(data_to_plot_4)
  
    min_max_table <- as.data.frame(rbind(c(0.10, 1.60),
                           c(8.0, 15.0),
                           c(0.0, 0.7),
                           c(0.9900, 1.004),
                           c(0.9, 15.5),
                           c(0.3, 2.0)))
    names(min_max_table) <- c('min', 'max')
    
    for (ii in 1:ncol(data_to_plot_4)) {
      newData_thisVar <- newData
      newData_thisVar[, ii] <- data_to_plot_4[, ii]
      newData_thisVar$predicted <- predict(model_pred(), newdata = newData_thisVar)
      newData_thisVar$quality <- data_to_plot_4_quality

      
      plot(newData_thisVar[, ii], newData_thisVar[, "quality"],
           xlab = names(newData_thisVar)[ii], ylab = "wine quality",
           #xlim = c(min(newData_thisVar[,ii], na.rm = TRUE), max(newData_thisVar[,ii], na.rm = TRUE)), 
           xlim = c(min_max_table$min[ii], min_max_table$max[ii]),
           ylim = c(0,10),
           col = "#969696")
      points(input_values[ii], predict_val_to_plot, col = color_pred(), pch = 19, cex = 3)
    }
  })

})

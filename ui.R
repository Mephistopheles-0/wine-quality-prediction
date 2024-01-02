
library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggfortify)

shinyUI(navbarPage(title = "Physicochemical Predictors of Wine Quality",
                   windowTitle = "Wine Quality App",
                   
                   tabPanel("Explore Variables",
                            fluidRow(
                              column(3,
                                     wellPanel(radioButtons("wine_type_explore", "Wine type:",
                                                  choiceNames = c("Red", "White"),
                                                  choiceValues = c("red", "white")),
                                     br(),
                                     radioButtons("variable_to_explore", "Variable to explore:",
                                                  choiceNames = c("Acidity", "Alcohol", "Chlorides", "Density", "Sugar", "Sulfates"),
                                                  choiceValues = c("acidity", "alcohol", "chlorides", "density", "sugar", "sulfates")),
                                     br(),
                                     radioButtons("exclude_outliers_explore", "Exclude outliers?",
                                                  choiceNames = c("No", "Yes"),
                                                  choiceValues = c("no", "yes"))
                                     
                                     )),
                              column(9,
                                     fluidRow(
                                       column(6,
                                              tableOutput("variable_summary")
                                              ),
                                       column(6,
                                              plotOutput("variable_boxplot")
                                              )
                                     ),
                                     fluidRow(
                                       column(6,
                                              plotOutput("variable_histogram")
                                              ),
                                       column(6,
                                              plotOutput("variable_qualitycor")
                                              )
                                     )
                            )
                   )),
                   tabPanel("Regression",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("wine_type_regression", "Wine type:",
                                             choiceNames = c("Red", "White"),
                                             choiceValues = c("red", "white")),
                                br(),
                                checkboxGroupInput("variables_regression", "Choose variables to include in model:", 
                                                   choices = c("acidity", "alcohol", "chlorides", 
                                                               "density", "sugar", "sulfates"),
                                                   selected = c("acidity", "alcohol", "chlorides", 
                                                                "density", "sugar", "sulfates")),
                                br(),
                                radioButtons("exclude_outliers_reg", "Exclude outliers?",
                                             choiceNames = c("No", "Yes"),
                                             choiceValues = c("no", "yes"))
                              ),
                              
                              mainPanel(
                                h4("Model fit results:"),
                                tableOutput("results_regression"),
                                br(),
                                textOutput("rsquared_regression"),
                                br(),
                                h4("Diagnostic plots:"),
                                plotOutput("plot_regression")
                              )
                            )),
                   tabPanel("Prediction",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("wine_type_prediction", "Wine type:",
                                             choiceNames = c("Red", "White"),
                                             choiceValues = c("red", "white")),
                                br(),
                                radioButtons("exclude_outliers_pred", "Exclude outliers?",
                                             choiceNames = c("No", "Yes"),
                                             choiceValues = c("no", "yes")),
                                sliderInput("slider_acidity", "Acidity", 0.10, 1.60, value = 0.10),
                                sliderInput("slider_alcohol", "Alcohol", 8.0, 15.0, value = 8.0),
                                sliderInput("slider_chlorides", "Chlorides", 0.0, 0.7, value = 0.0),
                                sliderInput("slider_density", "Density", 0.9900, 1.004, value = 0.9900),
                                sliderInput("slider_sugar", "Sugar", 0.9, 15.5, value = 0.9),
                                sliderInput("slider_sulfates", "Sulfates", 0.3, 2.0, value = 0.3)
                              ),
                              
                              mainPanel(
                                h4("Predicted quality:"),
                                br(),
                                textOutput("value_prediction"),
                                br(),
                                h4("Specified properties overlaid on raw data:"),
                                plotOutput("plot_prediction")
                              )
                            ))
))

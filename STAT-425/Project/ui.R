library(shiny)
library(data.table)
library(testthat)
library(gridExtra)
library(corrplot)
library(GGally)
library(ggplot2)
library(e1071)
library(dplyr) 
library(shinythemes)


shinyUI(navbarPage(theme = shinytheme("united"),"House Sales in King County",
                   # first tab panel
                   tabPanel("Exploratory Data Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("percent", "Pick the percentage of training set (60-90%)",
                                            min = 0.6, max = 0.9, value = 0.7, step = 0.01),
                                # textOutput("obs", "The number of observations in training set is"),
                                hr(),
                                
                                selectInput("pType", label = "Choose plot type:",
                                            choices=c("Barplots"="A","Histograms"="B",
                                                      "Correlation plot"="C","Highly correlated variables"="D"),
                                            selected = NULL),
                                hr(),
                                helpText(a(href="https://github.com/Yiming-Gao/STAT-425-project",
                                           target = "_blank","View the code"))
                              ),
                              
                              mainPanel(
                                h4(textOutput("obs")),
                                plotOutput("plot")
                              )
                            )),
                   
                   
                   
                   # second tab panel
                   navbarMenu("Model",
                              tabPanel("Linear Model",
                                       sidebarLayout(
                                         sidebarPanel(
                                           radioButtons("mType", "Model type",
                                                        c("Full model" = "m1",
                                                          "Reduced model by AIC&BIC"="m2"),
                                                        selected = NULL),
                                           
                                           hr(),
                                           
                                           helpText("The model is fitted on the training set, which is splittd
                                                    by the percentage you choose on the previous page. The prediction
                                                    is based on test set. Highly-skewed numeric variables have already been log transformed.")
                                           ),
                                         
                                         
                                         # # Show the main display
                                         # mainPanel(h3("Model Diagnostics"),
                                         #   plotOutput("residuals")
                                         # )
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Summary", verbatimTextOutput("summary")),
                                             tabPanel("Diagnostics Plots", plotOutput("residuals")),
                                             tabPanel("Prediction",
                                                      h4("Plot of residuals as a function of prediction"),
                                                      plotOutput("pred_res"),
                                                      h4(textOutput("rmse")))
                                           )
                                           
                                           
                                         ))
                                         ),
                              # secondary tab panel
                              tabPanel("Black Box Model",
                                       sidebarLayout(
                                         sidebarPanel(
                                           
                                           radioButtons("predType","Model type",
                                                        c("XGBoost" = "m3",
                                                          "Random Forest" = "m4"),
                                                        selected = NULL),
                                           helpText("It may take some time to run the program..."),
                                           checkboxInput("high_corr","Your predictors are highly correlated",value = FALSE),
                                           hr(),
                                           helpText(a(href="http://www.ibm.com/developerworks/library/ba-predictive-analytics2/",
                                                      target = "_blank","Learn more about predictive modeling")),
                                           helpText(a(href="https://epub.ub.uni-muenchen.de/9387/1/techreport.pdf",
                                                      target = "_blank","Learn more about importance measure"))
                                         ),
                                         
                                         # Show the main display
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Feature Importance", 
                                                      h5(textOutput("var_sel")),
                                                      plotOutput("imp")),
                                             tabPanel("Prediction",
                                                      h4("Plot of residuals vs. prediction"),
                                                      plotOutput("plot1"),
                                                      h4(textOutput("rmse1")))
                                           )
                                           
                                           
                                         )
                                       ))),
                   tabPanel("Make your prediction",
                            sidebarLayout(
                              sidebarPanel(h4("Please select or type: "),
                                radioButtons("modelType","Which model you want to use for prediction?",
                                                        c("Full model" = "model1",
                                                          "Reduced model" = "model2",
                                                          "XGBoost" = "model3",
                                                          "Random forest" = "model4"),
                                                        selected = NULL),
                                numericInput("grade","Building grade ", min = 1, max = 13, value = 1, step = 1),
                                numericInput("sqft_living","Square footage of total house",min=0,value = 1900, step = 10),
                                numericInput("sqft_basement","Square footage of basement (type 0 if no basement)",min=0,value = 0, step = 10),
                                numericInput("yr_built","When the house was built ",min=1900,max=2016,value = 2015, step = 1),
                                numericInput("bathroom","Number of bathrooms ",min=0, value = 2,step = 0.25),
                                numericInput("bedroom","Number of bedrooms ",value = 3,min=0,step = 1),
                                br(),
                                actionButton("button","Submit"),
                                p("Click the button to predict your house price!")
                                ),
                              
                              
                              
                              # main display
                              mainPanel(h3("House price prediction"),
                                        h4(textOutput("prediction")))
                            )),
                   tabPanel("About",
                            mainPanel(h3("House Price Explorer"),
                                      br(),
                                      p("This Shiny App is for visualizing and predicting house prices based on the 
                                        information from King County, which includes Seattle. It includes homes sold
                                        between May 2014 and May 2015. It has 19 house features plus the price, along with
                                        21613 observations."),
                                      br(),
                                      p("Due to the limited time, we picked out several most important features to make
                                        predictions. In particular, creating random forest model for such a large dataset
                                        is very time-consuming, so we simply pick out a subset just for illustration. The 
                                        actual accuracy should increase with larger sample size."),
                                      br(),
                                      p("It is my first time creating a Shiny App, there is still some improvement to be made. 
                                        For example, output a summary table for comparison, try some transformations to reduce
                                        the problems in diagnostics, and create more detailed, dynamic plots. I would be 
                                        appreciated if you give any suggestions!"),
                                      br(),
                                      helpText("Data Source:  ",a(href="https://www.kaggle.com/harlfoxem/housesalesprediction",
                                                                 target = "_blank","https://www.kaggle.com/harlfoxem/housesalesprediction")),
                                      br(),
                                      helpText("View the code:  ",a(href="https://github.com/Yiming-Gao/STAT-425-project",
                                                 target = "_blank","https://github.com/Yiming-Gao/STAT-425-project"))
                                      ))
                   
                   ))

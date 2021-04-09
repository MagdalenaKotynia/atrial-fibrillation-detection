#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(R.matlab)
library(readxl)
library(imputeTS)
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
library(e1071)
library(neuralnet)
library(corrplot)
library(ggplot2)
library(shiny)
library(rsconnect)
library(data.table)
library(outliers)
library(dplyr)
library(nnet)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Klasyfikator AF"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
            fileInput("file1", "Wybierz plik z danymi", accept = ".csv"),
            #checkboxInput("header", "Header", TRUE),
            tableOutput("contents")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"),
                       
            # plotOutput("distPlot")
            tabsetPanel(
                tabPanel("Klasyfikacja",
                         tabsetPanel(
                             tabPanel("Las losowy",
                                      sliderInput("trees",
                                                  "Wybierz liczbe drzew:",
                                                  min = 1,
                                                  max = 100,
                                                  value = 50),
                                        verbatimTextOutput("rfTrainResults"),
                                        verbatimTextOutput("rfTestResults"),
                                        verbatimTextOutput("rfPrediction")
                                      ),
                             tabPanel("Sieć neuronowa",
                                      sliderInput("neurons",
                                                  "Wybierz liczbe neuronow:",
                                                  min = 5,
                                                  max = 20,
                                                  value = 10),
                                      verbatimTextOutput("nnResults"),
                                      verbatimTextOutput("nnPrediction")
                                      #plotOutput("nnPlot")
                                      )
                         )),
                
                tabPanel("Informacje o danych",
                         tabsetPanel(
                             tabPanel("Boxploty",
                                      radioButtons("variable", "Wybierz ceche:",
                                                   c("SDSD",
                                                     "SDNN",
                                                     "RMSSD",
                                                     "pNN50",
                                                     "TRI",
                                                     #"TINN",
                                                     "SD1",        
                                                     "SD2",
                                                     "SD1SD2ratio",
                                                     "HR",
                                                     "med",
                                                     "qr",
                                                     #"pLF",
                                                     #"pHF",        
                                                     #"LFHFratio",
                                                     "VLF",
                                                     "LF",
                                                     "HF",
                                                     "PRstd",
                                                     "PRmean",
                                                     "RTstd",      
                                                     "RTmean",
                                                     "ApEn",
                                                     "CD"),
                                                   inline = TRUE),
                                      plotOutput("boxplot")),
                             tabPanel("Korelacje",
                                      checkboxGroupInput("variables", "Korelacje",
                                                    list("classification",
                                                         "SDSD",
                                                         "SDNN",
                                                         "RMSSD",
                                                         "pNN50",
                                                         "TRI",
                                                         #"TINN",
                                                         "SD1",        
                                                         "SD2",
                                                         "SD1SD2ratio",
                                                         "HR",
                                                         "med",
                                                         "qr",
                                                         #"pLF",
                                                         #"pHF",        
                                                         #"LFHFratio",
                                                         "VLF",
                                                         "LF",
                                                         "HF",
                                                         "PRstd",
                                                         "PRmean",
                                                         "RTstd",      
                                                         "RTmean",
                                                         "ApEn",
                                                         "CD"),
                                                    selected = list("classification",
                                                                    "HR"),
                                                   inline =TRUE),
                                      plotOutput("corrplot")
                                      
                             )
                             
                         )))
            
        )
        
    )
)
)

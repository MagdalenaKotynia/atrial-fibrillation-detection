#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


#WD <- setwd('C:/Users/magda/OneDrive/Dokumenty/Inżynierka/libs/mcode/database/myDataSet/HRV')
#load data from file
HRV <- read_excel("HRV.xlsx")
#replace NA values with means 
HRV <- na_mean(HRV)
HRV <- as.data.frame(HRV)
#WD <- setwd('C:/Users/magda/OneDrive/Dokumenty/VII sem/helloR')
exampleFeatures <- read.csv("test_data2.csv")

outliers <- function(x) {
    
    Q1 <- quantile(x, probs=.25)
    Q3 <- quantile(x, probs=.75)
    iqr = Q3-Q1
    
    upper_limit = Q3 + (iqr*3)
    lower_limit = Q1 - (iqr*3)
    
    x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
    for (col in cols) {
        df <- df[!outliers(df[[col]]),]
    }
    df
}
varsToSelect <- c("SDSD",
  "SDNN",
  "RMSSD",
  "pNN50",
  "TRI",
  "SD1",        
  "SD2",
  "SD1SD2ratio",
  "HR",
  "med",
  "qr",
  "VLF",
  "LF",
  "HF",
  "PRstd",
  "PRmean",
  "RTstd",      
  "RTmean",
  "ApEn",
  "CD")
HRV <- HRV %>% dplyr::select(varsToSelect, classification)
HRV <- remove_outliers(HRV)

#split data to train and test sets
trainIndex <- createDataPartition(HRV$classification, p = .8, 
                                  list = FALSE, 
                                  times = 1)
HRVTrain <- HRV[ trainIndex,]
HRVTest  <- HRV[-trainIndex,]

xTrain = HRVTrain[,1:ncol(HRVTrain)-1]
yTrain = HRVTrain[,ncol(HRVTrain)]
yTrain_notFactor = yTrain
yTrain = as.factor(yTrain)


xTest = HRVTest[,1:ncol(HRVTest)-1]
yTest = HRVTest[,ncol(HRVTest)]
yTest = as.factor(yTest)

#HRV$classification <- recode_factor(HRV$classification, "0" = "NIE","1" = "TAK")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    dane <- reactive(HRV)
    testX <- reactive(xTest)
    testY <- reactive(yTest)
    trainX <- reactive(xTrain)
    trainY <- reactive(yTrain)
    trainY_notFactor <- reactive(yTrain_notFactor)
    trainData <- reactive(HRVTrain)
    testData <- reactive(HRVTest)
    exampleData <- reactive(exampleFeatures)

    output$rfTrainResults <- renderPrint({
        # trees <- input$trees
        # rf <-randomForest(trainX(), trainY(), ntree = trees, keep.forest = TRUE)
        cat("Wyniki treningu lasu:\n")
        # rf
        rf <- trainRf()
        rf
        
    })
    output$rfTestResults <- renderPrint({
        testResults <- testRf()
        cat("\nDokladnosc testowa:\n")
        testResults
    })
    trainRf <- reactive({
        trees <- input$trees
        rf <-randomForest(trainX(), trainY(), ntree = trees, keep.forest = TRUE)
        return(rf)
    })
    
    testRf <- reactive({
        rf <- trainRf()
        pred <- predict(rf, testX())
        testAccuracyRf <- sum(1*(as.numeric(pred) == as.numeric(testY())))/nrow(testX())
        return(testAccuracyRf)
    })
    
    
    output$rfPrediction <- renderPrint({
        
        data <- features()
        #features <- as.data.frame(features)
        data <- data[,1:ncol(data)-1]
        rf <- trainRf()
        pred <- predict(rf, data)
        levels(pred)[1] <- 'NIE'
        levels(pred)[2] <- 'TAK'
        cat("Czy wystepuje migotanie przedsionkow?\n")
        cat(as.character(levels(pred))[pred])
        
    })
    
    # nnTraining <- reactive({
    #     neurons <- input$neurons
    #     n <- names(dane())
    #     f <- as.formula(paste("classification ~", paste(n[!n %in% "classification"],
    #                                                     collapse = "+")))
    #     nn <- neuralnet(formula = f, data = trainData(), linear.output = FALSE, 
    #                     hidden = neurons, threshold = 0.2)
    #     
    #     nn.results <- neuralnet::compute(nn, testX())
    #     results <- data.frame(actual = testY(), prediction = nn.results$net.result)
    # 
    #     results
    #     
    #     return(results)
    # })
    nnTraining <- reactive({
        neurons <- input$neurons
        n <- names(dane())
       
        trainData <- trainData()
        f <- paste(n[!n %in% "classification"],collapse = "+")  
        set.seed(2)
        model <- nnet(as.factor(classification)~.,trainData, size = neurons)
        return(model)
    })
    
    output$nnResults <- renderPrint({
        nnModel <- nnTraining()
        data <- testData()
        yTest <- testY()
        results <- predict(nnModel,data,type="class")
        mtab <-table(results,yTest)
        conf <- confusionMatrix(mtab, positive = "1")
        
        cat("Wynik testu sieci neuronowej:")
        conf
        
    })
    
    output$nnPrediction <- renderPrint({
        data <- features()
        data <- data[,1:ncol(data)-1]
        nn <- nnTraining()
        pred <- predict(nn,data,type = "class")
        if (pred == 1)
            pred = "TAK"
        else if (pred == 0)
            pred = "NIE"
        cat("Czy wystepuje migotanie przedsionkow?\n")
        cat(pred)
        
    })
    
   

    
    # output$nnPlot <- renderPlot({
    #     nn <- nnTraining()
    #     plot(nn)
    # })
    
    output$contents <- renderTable({

        data <- features() #inne nazwy 
        data_t <- transpose(data)
        
        colnames(data_t) <- rownames(data)
        rownames(data_t) <- colnames(data)
        colnames(data_t) <- ("Wartosc")
       
        as.data.table(data_t, keep.rownames = TRUE)
    })
    
    features <- reactive({
        inFile <- input$file1
        
        if (is.null(inFile)) {
            features <- exampleData()
        }   
            #return(inFile)
        else {
        features <- read.csv(inFile$datapath)
        }
        features <- features[,2:ncol(features)]
        return(features)
        
    })
    
    output$boxplot <- renderPlot({
         #dane <- reactive(recode_factor(HRV$classification, "0" = "NIE","1" = "TAK"))

        y = input$variable
        data = dane()
        x = factor(data$classification)
        levels(x)[1] <- 'NIE'
        levels(x)[2] <- 'TAK'
        (box <- ggplot(dane(), aes(x = x, y = get(y))) + 
                geom_boxplot() +
                stat_boxplot(coef = 3))
        stats <- ggplot_build(box)$data
        (box <- ggplot(dane(), aes(x = x, y = get(y))) + 
                geom_boxplot(color = "blue", fill = "red", outlier.colour = NA) +
                coord_cartesian(ylim = c(min(stats[[1]][["ymin"]]), 
                                         max(stats[[1]][["ymax"]]))) +
            labs(x = "Czy migotanie przedsionków?") +
                labs(y = y))
 
    })
    
    output$corrplot <- renderPlot({
        
    y = as.character(input$variables, collapse = ",")
     
        
        corrData <- reactive(HRV %>% dplyr::select(y))
        #(corrData <- dane() %>% select(get(y)))

        correlation <- cor(corrData())
        round(correlation, 2)
        corrplot(correlation, type = "upper", order = "hclust", 
                 tl.col = "black", tl.srt = 45)
    })
    

    


})

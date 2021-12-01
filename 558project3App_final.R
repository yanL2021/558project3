library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)
library(mathjaxr)
library(tidyverse)
library(caret)
library(rsample)
library(rpart)
library(rpart.plot)
library(pROC)

data_whole<-read_csv("diabetes_g.csv")

ui <- dashboardPage(
    dashboardHeader(title = "Diabetes Prediction"),
  

    
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    add_busy_spinner(spin = "radar",position="full-page"),
    menuItem("About",tabName = "About",icon = icon("address-card")),
    menuItem("Data Exploration",tabName = "Data Exploration",icon = icon("th"),
             #data select
             selectizeInput(
               inputId ="select1",
               "Select a variable",
               choices = names(data_whole[-9])
             ),
             sliderInput(inputId = "slider1",
                         label = "filter",
                         min = 1,
                         max = 5,
                         value = c(1,5)),
             #summary
             radioButtons(
               "RB1",
               label="type of summary",
               choices = list(
                 "Mean",
                 "Median")),
             menuSubItem("Summary", tabName = "Summary",icon = icon("play-circle")),
             
             #plots
             radioButtons(
               "RB2",
               label="type of plot",
               choices = list(
                 "Density",
                 "Boxplot")),
             menuSubItem("Plot", tabName = "Plot",icon = icon("play-circle"))
    ),
    menuItem("Modeling",tabName = "Modeling",icon = icon("th"),
             
             checkboxGroupButtons(
               inputId = "allInput",
               label = "Variables in Models:",
               choices = "ALL / NONE",
               size = "sm",
               selected = "ALL / NONE"
             ),
             
             checkboxGroupInput(inputId ="group", label = "",
                                choices = names(data_whole[,-9])
             ),
             sliderInput(inputId = "split",
                         label = "proportion of data",
                         min = 0,
                         max = 1,
                         value = 0.8,
                         step = 0.01),
             actionButton("Analyze",h5("Analyze!")),
             
             menuSubItem("ModelingIntro", tabName = "ModelingInfo",icon = icon("book")),
             menuSubItem("Logistic regression", tabName = "glm",icon = icon("play-circle")),
             menuSubItem("classification tree", tabName = "classification",icon = icon("play-circle")),
             menuSubItem("random forest", tabName = "random_forest",icon = icon("play-circle")),
             menuSubItem("Performance", tabName = "Performance",icon = icon("play-circle")),
             menuSubItem("Prediction", tabName = "Prediction")
    ), 
    menuItem("Data",tabName = "Data",icon = icon("th"),
             menuSubItem("Data_output", tabName = "Output"))
    
  )),  
  
  
  dashboardBody(
    tabItems(
      #About
      tabItem(tabName = "About",
              h3("Purpose: Predict the onset of diabetes based on diagnostic measures"),br(),
              h3("Data Source: Pima Indians Diabetes Database"),
              h4("The dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective of the dataset is to diagnostically predict whether or not a patient has diabetes, based on certain diagnostic measurements included in the dataset. Several constraints were placed on the selection of these instances from a larger database. In particular, all patients here are females at least 21 years old of Pima Indian heritage."),
              tags$a(href="https://www.kaggle.com/uciml/pima-indians-diabetes-database","Pima Indians Diabetes Database"),
              h3("App functions:"),
h4("1. Exploratory data analysis of the dataset"),
h4("2. Compare the performance of three models in predicting whether or not the patients in the dataset have diabetes or not."),
h4("3. Predict the probability of a patient have diabetes by user input values"),
h4("4. Save the data used in modeling as a csv file."),
img(src = "diabetes.jpg")
              ),
      #replace title = "Summary"
      tabItem(tabName = "Summary",
              h2("Summary for Data Exploration"),
              fluidRow(
                box(title = "Summary",  width = 10,  
                    solidHeader = TRUE, collapsible = FALSE,
                    tableOutput("table")))),
      #replace title = "Plot"
      tabItem(tabName = "Plot",
              h2("Plot for Data Exploration"),
              fluidRow(
                box(title = "Plot",  width = 10,  
                    solidHeader = TRUE, collapsible = FALSE,
                    plotOutput("dataPlot")))),
      
      tabItem(tabName = "ModelingInfo", 
              h2("Three modeling approaches:"),
              h3("Logistic Regression"),
              h4("We try to fit this data with a logistic regression model, since the response, Outcome, is binary. The observations are different patients which are independent of each other. There is no multicollinearity among the predictors based on VIF. Logistic regression is easier to implement, interpret, and very efficient to train. Logistic Regression needs that independent variables are linearly related to the log odds. This linear relationship can be written in the following mathematical form:"),
              uiOutput('formular'),
              h4("There are some situations where logistic regression might not perform well. One such situation is complete (or quasi-complete) separation of the data. This situation happens when the outcome variable separates a predictor completely. This leads to perfect prediction of the outcome by the predictor. In such a case, logistic regression may produce unreasonable over-inflated estimates of regression coefficients."),br(),
              h3("Classification Tree"),
              h4("One main advantage of Classification Tree is that they can be displayed graphically, and are easily interpreted even by a non-expert - this is especially true for small trees. The reason is that trees are very easy to explain to people since they more closely mirror human decision-making. Also, trees can easily handle categorical predictors without the need to create dummy variables. Since Outcome is binary, we applied a Classification Tree to fit our data. However, there are also some disadvantage of Classification Tree. Small changes in data can vastly change tree. Greedy algorithm necessary and need to prune usually."),br(),
              h3("Random Forest"),
              h4("Random Forest is based on the bagging algorithm and uses Ensemble Learning technique. Random forests provide an improvement over bagging by decorrelating the trees. It forces each split to consider only a subset of the predictors. 
                 Random Forest algorithm is very stable. Even if a new data point is introduced in the dataset, the overall algorithm is not affected much since the new data may impact one tree, but it is very hard for it to impact all the trees.
                 A disadvantage of random forest is that the resulting model is often difficult or impossible to interpret, as we are averaging many trees rather than looking at a single tree. We can still compute variable importance scores.")),
      
      tabItem(tabName = "glm", 
              h3("Logistic regression Fit statistics on the training data"),
              fluidRow(
                box(title = "",  width = 10,  
                    solidHeader = TRUE, collapsible = FALSE,
                    verbatimTextOutput("glmSummary")))),
      
      tabItem(tabName = "classification", 
              h3("Classification decision tree on the training data"),
              fluidRow(
                box(title = "",  width = 10,  
                    solidHeader = TRUE, collapsible = FALSE,
                    verbatimTextOutput("treeSummary")),
                box(title = "Classification decision tree",  width = 10,  
                    solidHeader = TRUE, collapsible = FALSE,
                    plotOutput("treePlot")))),
      
      tabItem(tabName = "random_forest", 
              h3("Random forest tunning and variable importance on the training data"),
              fluidRow(
                box(title = "",  width = 10,  
                    solidHeader = TRUE, collapsible = FALSE,
                    verbatimTextOutput("randomforest")),
                box(title = "Variable Importance Plot",  width = 10,  
                    solidHeader = TRUE, collapsible = FALSE,
                    plotOutput("importancePlot")))),
      
      tabItem(tabName = "Performance", 
              h2("Evaluate three models' performance on the test data"),
              fluidRow(
                box(title = "",  width = 10,  
                    solidHeader = TRUE, collapsible = FALSE,
                    tableOutput("performance")))),
      
      tabItem(tabName = "Prediction", 
              h2("Prediction based on random forest with all the variables"),
              numericInput("Pregnancies", label = "Pregnancies", value = 0),
              numericInput("Glucose", label = "Glucose", value = 0),
              numericInput("BloodPressure", label = "BloodPressure", value = 0),
              numericInput("SkinThickness", label = "SkinThickness", value = 0),
              numericInput("Insulin", label = "Insulin", value = 0),
              numericInput("BMI", label = "BMI", value = 0),
              numericInput("DiabetesPedigreeFunction", label = "DiabetesPedigreeFunction", value = 0),
              numericInput("Age", label = "Age", value = 0),
              actionButton("P",h5("Predict")),
              h3("Probability of diabetes (1) or not (0)"),
              fluidRow(box(title = "",  width = 10,  
                  solidHeader = TRUE, collapsible = FALSE,
                  tableOutput("predication")))),

      tabItem(tabName = "Output",
              h2("The data being used during modeling"),br(),
              h3("Please subset the data in modeling menu"),
              fluidRow(box(title = "",  width = 10, length=10, 
                           solidHeader = TRUE, collapsible = TRUE,
                           tableOutput("Data_Output"))),
              downloadButton(
                outputId = "downloadData",
                label = "Save as a csv",
                icon = icon("download")))
    )
  )
  
)

data_whole<-read_csv("diabetes_g.csv")


server <- function(input, output, session) {
  
  observe({updateSliderInput(session, inputId = "slider1", 
                    min = min(select(data_whole,input$select1)),
                    max = max(select(data_whole,input$select1)),
                    value =c(min(select(data_whole,input$select1)),max(select(data_whole,input$select1)) ))})
  #get new data based on the selected variable
  getData <- reactive({
    
    data_whole$Outcome<-as.factor(data_whole$Outcome)
    newData <- data_whole %>% filter(!!sym(input$select1) <= input$slider1)
  })
  
  #summary table
  output$table <- renderTable({
    #get new data 
    newData <- getData()
    
    if(input$RB1=="Mean"){
      newData %>% group_by(Outcome) %>%
        summarise(
          Avg = round(mean(!!sym(input$select1)), 0),
          Sd = round(sd(!!sym(input$select1)), 0))
    }else if(input$RB1=="Median"){
      newData %>% group_by(Outcome) %>%
        summarise(
          Median = median(!!sym(input$select1)),
          IQR = round(IQR(!!sym(input$select1)), 0))
    }
  })
  
  output$dataPlot <- renderPlot({
    
    newData <- getData()
    
    if(input$RB2=="Density"){
      g1 <- ggplot(newData, aes(x = !!sym(input$select1), fill = Outcome))
      g1 + geom_density(alpha = 0.5) +labs(y = "Density", x = input$select1) + theme_bw() + theme(legend.position = "none")
    }else if(input$RB2=="Boxplot"){
      g2 <- ggplot(newData, aes(x = Outcome, y = !!sym(input$select1))) 
      g2 + geom_boxplot(aes(fill = Outcome)) + theme_bw() + theme(legend.position = "none") + labs(y = input$select1)
    }
  })
  
  # update all/none group 
  observe({
    x <- input$allInput
    if (!is.null(x)) {
      x <- names(data_whole[,-9])
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      inputId ="group",
      label = NULL, 
      choices = names(data_whole[,-9]),
      selected = x
    )
  })
  
  
  output$formular <- renderUI({
    withMathJax(
      helpText('
               $$\\log {\\frac {p}{1-p}}=\\beta _{0}+\\beta x$$'))
  })
  
  #create a new dataset with selected variables
  Data_model<-eventReactive(input$Analyze,{
    
    data_whole$Outcome<-as.factor(data_whole$Outcome)

    data_selected<-select(data_whole,c(input$group, "Outcome"))
    
    set.seed(14)
    index <- initial_split(data_selected,
                           prop = input$split)
    train <- training(index)
    test <- testing(index)
    list(train,test,data_selected)
    
  })
  
  ### logistic regression model
  logistic <- reactive({
    #fit training data with glm
    glm_fit <-
      glm(Outcome ~ ., data = Data_model()[[1]], family = "binomial")
    
    # predicted probability of glm
    glm_prob <-
      predict(glm_fit, newdata = Data_model()[[2]], type = "response")
    
    # predicted outcome of glm
    glm_pred <- rep(0, length(glm_prob))
    glm_pred[glm_prob > 0.5] <- 1
    
    #glm performance
    glm_conf <-
      confusionMatrix(data = factor(glm_pred) ,
                      reference = Data_model()[[2]]$Outcome)  #confusion matrix
    accuracy_glm <- glm_conf$overall[[1]] #accuracy
    
    roccurve_glm <-
      roc(response = Data_model()[[2]]$Outcome, predictor = glm_prob)
    auc_glm <- auc(roccurve_glm)
    
    list(glm_fit, accuracy_glm, auc_glm)
    
  })
  
  #create glmSummary text info
  output$glmSummary <- renderPrint({
    summary(logistic()[[1]])
  })
  
  
  #### Classification 
  tree <- reactive({
    #fit training data with tree
    tree_class <- rpart(
      Outcome ~ .,
      data = Data_model()[[1]],
      method = 'class',
      parms = list(split = "information"),
      control = rpart.control(
        xval = 5,
        minbucket = 2,
        cp = 0
      )
    )
    #prune final tree model
    cp <- as.data.frame(tree_class$cptable)
    tree_class_final <- prune(tree_class, cp = filter(cp, xerror==min(cp$xerror))$CP)#used minimum
    
    #classification tree performance
    tree_pred <- predict(tree_class_final, newdata=Data_model()[[2]], type = "class")
    accuracy_tree<-mean(tree_pred == Data_model()[[2]]$Outcome)
    tree_prob <- predict(tree_class_final, newdata=Data_model()[[2]], type = "prob")
    roccurve_tree <- roc(response = Data_model()[[2]]$Outcome, predictor = tree_prob[,2])
    auc_tree<-auc(roccurve_tree)
    
    list(tree_class,tree_class_final,accuracy_tree,auc_tree)
  })
  
  #create treeSummary text info
  output$treeSummary <- renderPrint({
    
    printcp(tree()[[1]])
   
  })
  #create treePlot
  output$treePlot <- renderPlot({

    rpart.plot(tree()[[2]]) #tree plot
    
  })
  
  
  ### Random forest modeling
  rf <- reactive({
    #fit training data with rf
    control <- trainControl(method='cv', 
                            number=5, 
                            search='grid')
    
    tunegrid <- expand.grid(.mtry = (1:10)) 
    
    rf_gridsearch <- train(Outcome~ ., 
                           data = Data_model()[[1]],
                           method = 'rf',
                           metric = 'Accuracy',
                           tuneGrid = tunegrid)
    
    #randomForest performance
    rf_best <- train(Outcome~ ., 
                     data = Data_model()[[1]],
                     method = 'rf',
                     metric = 'Accuracy',
                     tuneGrid = rf_gridsearch$bestTune)
    
    randomF_best_pred <- predict(rf_best, newdata=Data_model()[[2]], type = "raw")
    accuracy_randomF_best<-mean(randomF_best_pred == Data_model()[[2]]$Outcome)
    
    randomF_best_prob <- predict(rf_best, newdata=Data_model()[[2]], type = "prob")
    
    roccurve_randomF <- roc(response = Data_model()[[2]]$Outcome, predictor = randomF_best_prob[,2])
    auc_randomF<-auc(roccurve_randomF)
    
    list(rf_gridsearch,accuracy_randomF_best,auc_randomF,rf_best)
    
  })
  
  #create randomforest Summary text info
  output$randomforest <- renderPrint({
    
    print(rf()[[1]])
    
  })
  #create treePlot
  output$importancePlot <- renderPlot({
    plot(varImp(rf()[[1]])) #importance plot
  })
  
  #Performance table
  output$performance <- renderTable({
    #bind accuracy and auc of ROC
    accuracy<-rbind(logistic()[[2]],tree()[[3]],rf()[[2]])
    auc<-rbind(logistic()[[3]],tree()[[4]],rf()[[3]])
    performance<-as.data.frame(cbind(accuracy,auc))
    colnames(performance)<-c("accuracy","auc")
    rownames(performance)<-c("glm", "tree","randomForest")
    performance
  },rownames = TRUE)
  
  #actionbutton for prediction
  Data_predict<-eventReactive(input$P,{
    DataInput<-cbind(input$Pregnancies, input$Glucose, input$BloodPressure, input$SkinThickness, input$Insulin, input$BMI, input$DiabetesPedigreeFunction, input$Age)
    colnames(DataInput)<-c("Pregnancies","Glucose","BloodPressure", "SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Age")
    DataInput
  })

    #prediction
  output$predication <- renderTable({
     predict(rf()[[4]], newdata=Data_predict(), type = "prob")
    
  })
  
  #modeling data output
  output$Data_Output<- renderTable({
    Data_model()[[3]]
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("OutputData", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(Data_model()[[3]], file)
    }
  )

}

shinyApp(ui, server)
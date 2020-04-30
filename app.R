library(rsconnect)
rsconnect::setAccountInfo(name="justin-mcguire", token="55D57412D5F5EEC3655ECE5F564D5892", secret="IOFSGrWpvYLR0ItPAQuvVm+EaVM4BL9MuNswd8W8")
#Load Libraries
library(shiny)
library(datasets)

#Create a new variable to hold the dataset
mpgData <- mtcars

#Modify dataframe
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
mpgData$cyl<-as.factor(mpgData$cyl)
mpgData$gear<-as.factor(mpgData$gear)
mpgData$carb<-as.factor(mpgData$carb)
mpgData$vs<-as.factor(mpgData$vs)

#Define a server for the Shiny app
server<-shinyServer(function(input, output) {
  
  #Get all the checked values separated by + (example cyl+hp), if nothing is checked make it 1
  checkedVal <- reactive({
    perm.vector <- as.vector(input$checkGroup)
    predForm<-ifelse(length(perm.vector)>0,
                     predictors<-paste(perm.vector,collapse="+"),
                     "1")
    lmForm<-paste("mpg~",predForm,sep="") 
    
  }) 
  
  fitModel<-reactive({
    fitFormula<-as.formula(checkedVal())
    lm(fitFormula,data=mpgData)
  })
  
  output$caption <- renderText({
    checkedVal()
  })
  
  #Print the coeffecients of the regression model
  output$fit <- renderPrint({
    summary(fitModel())$coef
  })
  
  #Plot Diagnostics for the generated regression model
  output$mpgPlot<-renderPlot({
    par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
    plot(fitModel(),sub.caption="Diagnostic Plots")
    
  })
})
#Load Libraries
library(shiny)
library(datasets)
library(dplyr)

# Define UI for miles per gallon application
ui<-shinyUI(fluidPage(
  
  # Application title
  titlePanel("Linear Regression of Miles/Gallon(mpg) with One or More Predictors"),
  
  # Sidebar with controls to select the variable to plot against
  # mpg and to specify whether outliers should be included
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("checkGroup", 
                         label = h3("Predictor/s"), 
                         choices=names(select(mtcars,-mpg)),
                         selected = "cyl"
      ),
      width=3
    ),
    
    
    mainPanel(
      h4(textOutput("caption")),
      verbatimTextOutput("fit"),
      plotOutput("mpgPlot")
    )
  )
))
shinyApp(ui,server)


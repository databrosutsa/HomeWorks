library(shiny)
library(dplyr)
library(ggplot2)
library(foreign)
library(haven)
library(MASS)

ui <-  fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("myFile",
                "Upload SAS File",
                accept = c(".sas7bdat")
      ),
      uiOutput("uiX"),
      uiOutput("uiY"),
      uiOutput('uiModel'),
      uiOutput('uiRibbon'),
      uiOutput("uiScale"),
      conditionalPanel('input.model == "LOESS"',
                       uiOutput('uiSpan'))
    ),
    mainPanel(plotOutput("plot1")
      
    )
  )
)
server <- function(input, output){
  
  a1= reactive({
    if(is.null(input$myFile$datapath)) NULL
    else{
      read_sas(input$myFile$datapath)
    }
  })
 
  output$uiX <- renderUI({
    selectInput('x',
                'X-axis variable:',
                choices = c('Sales' = 'SALEQ',
                            'Cash' = 'CHEQ',
                            'R&D' = 'XRDQ',
                            'Assets' = 'ATQ',
                            'Profits' = 'OIADPQ',
                            'SG&A' = 'XSGAQ'),
                selected = 'Sales')
  }) 
  
  output$uiY <- renderUI({
    selectInput('y',
                'Y-axis variable:',
                choices = c('Sales' = 'SALEQ',
                            'Cash' = 'CHEQ',
                            'R&D' = 'XRDQ',
                            'Assets' = 'ATQ',
                            'Profits' = 'OIADPQ',
                            'SG&A' = 'XSGAQ'),
                selected = 'R&D')
  })
  
  output$uiModel <- renderUI({
    radioButtons('model',
                 'Choose the Model:',
                 choices = c('Linear Model',
                             'LOESS',
                             'Robust Linear',
                             'None'),
                 selected = 'LOESS')
  })
  
  output$uiSpan <- renderUI({
    if(is.null(a1())) NULL
    else{
      sliderInput('span',
                  'Span for LOESS',
                  min = 0,
                  max = 1,
                  value = 0.75,
                  step = NULL,
                  ticks = TRUE)
    }
  })
  
  output$uiRibbon <- renderUI({
    checkboxInput('ribbon',
                  'Standard Error Ribbon',
                  value = TRUE)
  })
  
  output$uiScale <- renderUI({
    selectizeInput("scale",
                   "Choose the scale",
                   choices = c("Levels",
                               "Log10"),
                   selected = "Levels")
  })
  
  output$plot1 <- renderPlot({

    if (is.null(a1())) {    
      text <- paste("Please upload a SAS data file (.sas7bdat extension) \n",
                                          "Make sure that it has the following variables: \n",
                                          "SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ")
    
    validate(
      need(!is.null(input$myFile), text)
    )
    }
    else{
      g1 <- ggplot(data = a1(),aes_string(x = input$x, y = input$y)) +
        geom_point() +
        labs(x = paste(input$x), "(million $)",
             y = paste(input$y), "(million $)")
      
      if(input$model == "LOESS"){
        return(g1 + geom_smooth(method = "loess", span = input$span))
      }
      if(input$model == "Linear Model"){
        return(g1 + geom_smooth(method = "lm"))
      }
      if(input$model == "Robust Linear"){
        return(g1 + geom_smooth(method = "rlm"))
      }
        else{
          return(g1)
        }
    }
  })
  

}



shinyApp(ui = ui, server = server)
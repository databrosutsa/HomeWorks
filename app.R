library(shiny)
library(dplyr)
library(ggplot2)
library(foreign)
library(haven)


ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("myFile",
                "Upload SAS File",
                accept = c(".sas7dbat")
      ),
      uiOutput('uiX'),
      uiOutput('uiY'),
      uiOutput('uiModel'),
      uiOutput('uiRibbon'),
      conditionalPanel('input.model == "LOESS"',
                       uiOutput('uiSpan')),

      selectInput('scale',
                  'Choose the scale',
                  choices = c('Levels',
                              'Log10'),
                  selected = 'Levels')
      
    ),
    mainPanel(
      conditionalPanel('input.ribbon' = TRUE,
                       plotOutput('p1')),
      conditionalPanel('input.ribbon' != TRUE,
                       plotOutput('p2'))
                         # get these on one plot
    )
  )
)

server = function(input, output){
  
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
                  value = 0, 1,
                  step = NULL,
                  ticks = TRUE)
    }
  })
  
  output$uiRibbon <- renderUI({
    checkboxInput('ribbon',
                  'Standard Error Ribbon',
                  value = TRUE)
  })
  
  output$p1 <- renderPlot({
    text <- paste("Please upload a SAS data file (sas7bdat extension) \n",
                  "Make sure that it has the following variables: \n",
                  "SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ")
    
    validate(need(!is.null(input$myFile), text)
             )
    
    if (is.null(a1())) NULL
    else{
      ggplot(data = a1(),aes_string(x = input$x, y=input$y))+
        geom_point()+
        geom_smooth(se=input$ribbon)
    }
  })
  
  output$p2 <- renderPlot({
    text <- paste("Please upload a SAS data file (sas7bdat extension) \n",
                  "Make sure that it has the following variables: \n",
                  "SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ")
    
    validate(need(!is.null(input$myFile), text)
    )
   if (is.null(a1())) NULL
   else{
     ggplot(data = a1(),aes_string(x = input$x, y=input$y))+
      geom_point()
  }
 })
 
}

shinyApp(ui = ui, server = server)
      
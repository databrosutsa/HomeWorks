## app.R ##
library(ggplot2)
library(MASS)
library(plotly)
library(pastecs)
library(sas7bdat)
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggthemes)

ui <- dashboardPage(
  
  dashboardHeader(title = "Apple Financials"),
  
  dashboardSidebar(
    
    fileInput("myFile",
              "Upload SAS Data:",
              accept = c(".sas7bdat")
    ),
    
    selectInput("xaxis", "X-axis:",
                choices = c('Sales' = 'SALEQ',
                            'Cash' = 'CHEQ', 
                            'Assets' = 'ATQ',
                            'Profits' = 'OIADPQ',
                            'R&D' = 'XRDQ', 
                            'SG&A' = 'XSGAQ'),
                selected = 'SALEQ'
    ),
    
    selectInput("yaxis", "Y-axis:",
                choices = c('Sales' = 'SALEQ',
                            'Cash' = 'CHEQ', 
                            'Assets' = 'ATQ',
                            'Profits' = 'OIADPQ',
                            'R&D' = 'XRDQ', 
                            'SG&A' = 'XSGAQ'),
                selected = 'XRDQ'
    ),
    
    selectInput("scale", "Choose the scale:",
                choices = c("Levels",
                            "Log 10"),
                selected = "Levels"),
    
    radioButtons("model", "Choose the model:",
                 choices = c("Linear Model",
                             "LOESS",
                             "Robust Linear",
                             "None"),
                 selected = "LOESS"),
    
    checkboxInput("se", "Standard Error Ribbon:", TRUE),
    
    sliderInput("loessSpan", "Span for LOESS", min  = 0, max = 1, value = .75)
    
    
  ),
  
  dashboardBody(
    
    
    plotOutput("myPlot")
    
  )
)


displayNames = function (x) {switch(x,'SALEQ' = 'Sales',
                 'CHEQ' = 'Cash', 
                 'ATQ' = 'Assets',
                 'OIADPQ' = 'Profits',
                 'XRDQ' = 'R&D', 
                 'XSGAQ' = 'XSGAQ')
}

# dataNames = c('SALEQ',
#               'CHEQ', 
#               'ATQ',
#               'OIADPQ',
#               'XRDQ', 
#               'XSGAQ')




server = function(input, output){

  ### Data Container 
  
  dataContainer <- reactive({
    if (is.null(input$myFile$datapath)) NULL
    else {
      read.sas7bdat(input$myFile$datapath)
    }
  })
  
  ### Interactive Slider
  
  output$sliderLOESS <- renderUI({
    if (input$model != 'LOESS') NULL
    else {
      sliderInput('loessSpan',
                  'Span for LOESS',
                  min = 0,
                  max = 1,
                  value = .75)
    }
  })
  
  ### Plotting
  
  output$myPlot <- renderPlot ({
    
    text = paste("X and Y variables have to be different")
    
    text2 = paste("Please upload a SAS data file (sas7bdat extension)\n",
                   "Make sure that it has the following variables:\n",
                  "SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ")
    
    
    ### Error validation

    validate(need(!is.null(dataContainer()), text2))
    
    validate(need(input$xaxis != input$yaxis, text))
    
    ### Conditionals   
    
    if (is.null(dataContainer())) {NULL}
    else {
      
      
      xlabel <- switch(input$xaxis, displayNames)
      
      ylabel <- switch(input$yaxis, displayNames)
      
  
      
      if (input$scale == 'Levels') {
        
      
        graph <- ggplot(data = dataContainer(),
                        aes_string(x = input$xaxis, y=input$yaxis))+
          
          labs(x = paste(displayNames(input$xaxis), "(million $)"),
               y = paste(displayNames(input$yaxis), "(million $)"))+
          theme_economist() 
        
      } else {
        
      
        graph <- ggplot(data = dataContainer(),
                        aes_string(x = input$xaxis, y=input$yaxis))+
          
          scale_x_log10()+
          scale_y_log10()+
          labs(x = paste(displayNames(input$xaxis), "(million $)"),
               y = paste(displayNames(input$yaxis), "(million $)"))+
          theme_economist() 
      }
      
      if(input$model == "Linear Model") return(
        
        graph +
          geom_point() +
          geom_smooth(method = "lm", se = input$se)
          
        
      )
      
      if (input$model == "LOESS") return(
        
        graph +
          geom_point() +
          geom_smooth(method = "loess", se = input$se, span = input$loessSpan)
          
        
      ) 
      
      if (input$model == "Robust Linear") return(
        
        graph +
          geom_point() +
          geom_smooth(method = "rlm", se = input$se)
          
        
      ) 
      
      if (input$model == "None") return(
        
        graph +
          geom_point()
          
      )
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
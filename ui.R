#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)



# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    # Application title
    titlePanel("PSE results exploration"),
    
    sidebarLayout(
      sidebarPanel(
        
        sliderInput(
          inputId = "UIredNoise",
          label = "red noise",
          min = 0,
          max = 100,
          value = c(0, 100)
        )
        
        ,
        sliderInput(
          inputId = "UIharshness",
          label = "harshness",
          min = 0,
          max = 99,
          value = c(0, 99)
        ),
        sliderInput(
          inputId = "nbpoints",
          label = "Sample size",
          min=1,
          max = 3180,
          value = 1500
          )
        ,
        checkboxInput("displayExtrema", 
                      label = "Display extrema in other dimensions ",
                      value = F)
        
        
        ,
        tableOutput("tabPoint")
        ,
        width = 3
      )
      
      ,
      
      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(plotlyOutput("nuagePlot1"))
        ,
        fluidRow(plotlyOutput("nuagePlot2"))
        , width=9
      )
    )
  )
)
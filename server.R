#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(scales)
library(RColorBrewer)

setwd("/home/chap/dev/iodisplayRshiny/")

dfpse <- read.csv("PSE1_6000genNoGraph.csv", sep = ";", dec = ",")

#remove first column which is constant 
dfpse <-  dfpse[,-1]
#remove last column with replications, since it's almsot constant 
# as indicated by : 
qplot(dfpse$evolution.samples)
dfpse <-  dfpse[,-7]



#assuming columns of outputs are propConf, propMaxi, propAnti, and meandDev

outputNames <- names(dfpse)[3:6]

# label for min and max of each output
labelsMinima <-  paste("min_",outputNames,sep = "")
labelsMaxima <-  paste("max_",outputNames,sep = "")
labelsOutputExtremas <- c(labelsMinima,labelsMaxima)




#determining bounds for data columns (maybe used later)
minis <- apply(dfpse,MARGIN = 2,min)
maxis <- apply(dfpse,MARGIN = 2,max)
bornes <- data.frame(names(dfpse), minis, maxis)






#function to interpolate color according to a vector
interpolColor <- function(vect){
  idxcol <- round(rescale(vect,to=c(0,100)))
  return(palette[idxcol])
}

fonfont <- list(
  family = "sans serif",
  size = 12,
  color = 'black')



shinyServer(function(input, output) {
#  options(warn = -1)
  subsetdfpse <- reactive({
    # make the sample
    currentdfpse <- sample_n(dfpse, size = input$nbpoints)
    # select data within user interface sliders values (for input) 
    currentdfpse[
      currentdfpse$redNoise >= input$UIredNoise[1]
      & currentdfpse$redNoise <= input$UIredNoise[2]
      & currentdfpse$harshness >= input$UIharshness[1]
      & currentdfpse$harshness <= input$UIharshness[2]
      ,]
    
  })
  
  
  
  output$nuagePlot1 <- renderPlotly({
    
    #update the subset beforre render  
    subsetdfpse <- subsetdfpse()
    otherDimsP1 <-  c("propAnti", "meanDev")
    idxminP1 <- sapply(subsetdfpse[,otherDimsP1],which.min)
    idxmaxP1 <- sapply(subsetdfpse[,otherDimsP1],which.max)
    pointsInteretP1 <- subsetdfpse[append(idxminP1,idxmaxP1),]
    pointsInteretP1$label <- c(paste("min_",otherDimsP1,sep = ""),paste("max_",otherDimsP1,sep = ""))
    
    displayExtrema <-  input$displayExtrema
    
    
    p2d<- plot_ly(subsetdfpse,  x = ~propConf, y=~propMaxi, 
                  type="scatter" ,
                  mode="markers", 
                  hoverinfo="text",
                  text = ~paste("", '</br>propConf: ', round(propConf, digits = 2), 
                                '</br> propMaxi: ', round(propMaxi, digits = 2),
                                '</br> propAnti: ', round(propAnti, digits  =2) ,
                                '</br> meanDev:', round(meanDev, digits = 2)),
                  showlegend=F,
                  marker = list(size = 3, 
                                opacity=0.8, 
                                line = list(width = 0.1, color="gray25") )
                  
                  ,width = 500, height = 400)
    
      if(input$displayExtrema){
      p2d <-  add_markers(p2d,
                  data=pointsInteretP1, type="scatter3d",
                  marker=list(size=6, opacity=0.8, color="red", text ),
                  text = ~label,
                  textposition = 'bottom right'
                  )
     
      }
      p2d
    
    
  })
  
  
  output$nuagePlot2 <- renderPlotly({
    
    #update the subset beforre render  
    
    subsetdfpse <- subsetdfpse()
    otherDimsP2 <-  c("propConf", "propMaxi")
    idxminP2 <- sapply(subsetdfpse[,otherDimsP2],which.min)
    idxmaxP2 <- sapply(subsetdfpse[,otherDimsP2],which.max)
    pointsInteretP2 <- subsetdfpse[append(idxminP2,idxmaxP2),]
    pointsInteretP2$label <- c(paste("min_",otherDimsP2,sep = ""),paste("max_",otherDimsP2,sep = ""))
    
    
    
    p2d2<- plot_ly(subsetdfpse,  x = ~propAnti, y=~meanDev, 
                  type="scatter",
                  mode="markers", 
                  hoverinfo="text",
                  text = ~paste("", '</br>propConf: ', round(propConf, digits = 2), 
                                '</br> propMaxi: ', round(propMaxi, digits = 2),
                                '</br> propAnti: ', round(propAnti, digits  =2) ,
                                '</br> meanDev:', round(meanDev, digits = 2)),
                  showlegend=F,
                  marker = list(size = 3, 
                                opacity=0.8, 
                                line = list(width = 0.1, color="gray25") )
                  
                  ,width = 500, height = 400)

    
    if(input$displayExtrema){
      p2d2 <-  add_markers(p2d2,
                          data=pointsInteretP2, type="scatter3d",
                          marker=list(size=6, opacity=0.8, color="red", text ),
                          text = ~label,
                          textposition = 'bottom right'
      )
      p2d2
    }
    p2d2
    
    
  })
  
  
  
  
  
  output$tabPoint<- renderTable({
    evd <- event_data("plotly_hover")
    req(evd)
    
    subsetdfpse<- subsetdfpse()
    currentConfig<- t(subsetdfpse[(evd$pointNumber+1), ])
    currentConfig
  }, colnames = T, rownames = T)
  
  
})


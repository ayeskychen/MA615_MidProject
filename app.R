#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#initialize
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyverse)
library(stringr)
require(gdata)
library(tuneR)
library(markdown)


#Load data
RedSox <- read_csv("RedSoxWeather.csv")
Celtics <- read_csv("CelticsWeather.csv")

RedSox$Year <- as.factor(RedSox$Year)
Celtics$Year <- as.factor(Celtics$Year)
RedSox <- RedSox %>% group_by(ATMP) %>% arrange(ATMP) %>% filter(ATMP<=104)
Celtics <- Celtics %>% group_by(ATMP) %>% arrange(ATMP) %>% filter(ATMP<=70)

#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'blue', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)


# UI for app
ui<-(pageWithSidebar(
  # title
  headerPanel("MA615 Midterm Project"),

  
  #input
  sidebarPanel
  (
    
    
    h3("Selection Panel"),
    
    # Horizontal line ----
    tags$hr(),
    
    
    # Input: Select what to display
    
    selectInput("dataset","Data:",
                choices =list(RedSox = "RedSox", Celtics = "Celtics"
                              ), selected=NULL),
    selectInput("attendance","Attendance:", choices = NULL),
    selectInput("variable","Variable:", choices = NULL),
    selectInput("plot.type","Plot Type:",
                list(scatterplot = "scatterplot",boxplot = "boxplot" )
    ),
    checkboxInput("show.points", "show points", FALSE),
    checkboxInput("show.line", "show line", FALSE),
    
    
    tags$hr(),
    
    h4("A Work by:"),
    h4("Dave, Sky, Tingrui, Xiang"),
    h6('Data Source from'),
    tags$img(src='ESPN.png',height = 50,width = 250),
    tags$img(src='Baseball.png',height = 50,width = 250),
    tags$img(src='Buoy.png',height = 50,width = 250),
    tags$img(src='Weather.png',height = 50,width = 250)
    
    
  ),
  
  # output
  mainPanel(
    h3(textOutput("caption")),
    #h3(htmlOutput("caption")),
    uiOutput("plot"), # depends on input,
    #fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("BarPlot1"), plotOutput("BarPlot2"))
    textOutput("text")
  )
))


# shiny server side code for each call
server<-(function(input, output, session){
  
  #update variable and
  #attendance based on the data

  observe({
    #browser()
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, "attendance", choices = var.opts[24])
    updateSelectInput(session, "variable", choices = var.opts[c(2,7,14,19,20,23)])
  })
  

 output$caption<-renderText({
      switch(input$plot.type,
             "boxplot" 	= 	"Boxplot",
             "scatterplot" =	"Scatterplot")
    })
 
    
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  
  output$text <- renderText({
    paste0("For variable ATMP, PRCP, WSPD, use scatterplot;", 
           "\n", 
           "for variable DN, Day_of_week, Year, use boxplot.")
  })
  

  #get data object
  get_data<-reactive({
    
    if(!exists(input$dataset)) return() # if no upload
    
    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()
    
    obj<-list(data=get(input$dataset),
              attendance=input$attendance,
              variable=input$variable
    )
    
    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    #make sure choices had a chance to update
    check<-function(obj){
      !all(c(obj$attendance,obj$variable) %in% colnames(obj$data))
    }
    
    if(check(obj)) return()
    
    
    obj
    
  })
  
  #plotting function using ggplot2
  output$p <- renderPlot({
    
    plot.obj <- get_data()
    
    #conditions for plotting
    if(is.null(plot.obj))
      return()
    
    #make sure attendance and variable have loaded
    if(plot.obj$attendance == "" | plot.obj$variable =="") return()
    
    #plot types
    plot.type <- switch(input$plot.type,
                        "boxplot" 	= geom_boxplot(),
                        "scatterplot" =	geom_point(color='black',alpha=0.5, position = 'jitter')
    )
    
    
    if(input$plot.type == "boxplot")	{		
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable,
                  y 		= plot.obj$attendance,
                  #color = plot.obj$DN,
                  fill 	= plot.obj$variable # let type determine plotting
                )
      ) + plot.type
      
      if(input$show.points == TRUE)
      {
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      
    } else { #selected scatterplot
     
      p <- ggplot(plot.obj$data,
                  aes_string(
                  x 	= plot.obj$variable,
                  y 	= plot.obj$attendance,
                  fill 	= plot.obj$variable
                  #color 	= as.factor(plot.obj$variable)
                )
      )  + plot.type

      input$show.points==FALSE

      if(input$show.line==TRUE)
      {
        p<-p+ geom_smooth(method = lm  ,se = FALSE)
      }
      
    }
    
    p<-p+labs(
      fill 	= input$variable,
      x 		= "",
      y 		= input$attendance
    ) #  +
      # ylim( 0, ifelse( plot.obj == RedSox, 40000, 20000) )
    print(p)
  })
  

  
  
})


# Run the application 
shinyApp(ui = ui, server = server)


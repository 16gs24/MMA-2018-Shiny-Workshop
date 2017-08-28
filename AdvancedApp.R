#load libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(rvest)
library(data.table)
library(lubridate)
library(plotly)

#initialize our team
source("ScrapingData.R")
Team <- "ATL"
basic<-data.frame(moneyball(Team, 2004, 2016))
write.csv(basic,"basic.csv")

#begin by building out UI. We will use a sidebar for selections and 
# a main bar that has a scatterplot, summary stats and the head
ui <- shinyUI(
  #lets make dynamic pages
  fluidPage(
    #set theme & name
    headerPanel(paste('MMA Shiny Tutorial',Team,'Attendance',sep=" ")),
    theme=shinytheme('readable'),
    #build out our side panel
    sidebarLayout(
      sidebarPanel("dropdowns not for plotly",
        #Input for the x variable of scatterplot
        selectInput('xcol', 'X Variable (Continuous only)',
                    c('Date','Game','Rank','GB','Streak','Year','Month')),
        #Input for the color of the scatterplot
        selectInput('color','Color By (factors only)',
                    c("Team","Day","WinPitch","LosePitch","Weekday","Weekend")),
        #Slider for a selection of months
        sliderInput(inputId='month',
                    label="Choose Months to View",
                    value=c(4,10),min=3,max=11),
        #checkbox for what day of the week we want to view
        checkboxGroupInput(
          inputId='dayofweek',
          label='Day of Week',
          choices = unique(basic$Weekday),
          selected = unique(basic$Weekday)),
        checkboxInput('jitter','Jitter')
      ) ,
      #enough inputs. what outputs do we want to look at.
      mainPanel(tabsetPanel(
        tabPanel("Plotly",
                 plotlyOutput('plotly')),
        tabPanel("ggplot Multivariate",
                 plotOutput('scatter'),
        #Show Summary stats
        verbatimTextOutput('summary')),
        tabPanel("ggplot Univariate",
                 plotOutput('Xhistogram'),
                 plotOutput('AttendanceHistogram')),
        #show the head of our data
        tabPanel("Raw Data",tableOutput('raw')
      )))
    )
  )
)

# Server Code
server <-   shinyServer(
  function(input, output) {
    #begin by reactively filtering the data
    #filter for day of the week from selection, and month from slider
    filtered <- reactive({
      basic %>%
        filter(
          Weekday %in% input$dayofweek &
            #We need to select more than the small input slider
            #and less than the largest
            Month>=input$month[1] & 
            Month<=input$month[2])
    })
    #first output is our summary statistics  
    output$summary <- renderPrint({
      #to get our data reactively filtered, we need to redefine our 
      #data source inside render*()
      data <- filtered() 
      summary(data$Attendance)
    })
    
    #next we will get a table of the head of 8 games
    output$raw <- renderTable({
      data <- filtered()
      return(data)
    })
    
    #finally build a ggplot with scatter plot of Attendance vs our choice
    output$scatter <- renderPlot({
      data <- filtered()
      #we need aes_string since input$xcol isn't inside data
      viz <- ggplot(data=data,aes_string(x=input$xcol, 
                                  y='Attendance',
                                  col=input$color))+  
        theme_set(theme_gray(base_size = 16))+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
        scale_fill_discrete(palette="Blues")+
        scale_y_continuous(limits=c(20000,41000))+
        ggtitle("ggplot makes static plots")
      if (input$jitter==TRUE){
        viz + geom_jitter()
      } else {
        viz + geom_point()
      }
    })
    
    #PLOTLY
    output$plotly <- renderPlotly({
      data <- filtered()
      p <- plot_ly(data=data,
                   x=~Streak,
                   y=~get(input$xcol),
                   z=~Attendance,
                   color=~get(input$color)) %>%
      layout(title="Plotly can create interactive plots")
    })
    
    #X HISTOGRAM
    output$Xhistogram <- renderPlot({
      data <- filtered()
      #we need aes_string since input$xcol isn't inside data
      ggplot(data=data,aes_string(x=input$xcol))+
        geom_histogram()+  
        theme_set(theme_gray(base_size = 16))+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
        scale_fill_discrete(palette="Blues")
    })
    
    #ATTENDANCE HISTOGRAM
    output$AttendanceHistogram <- renderPlot({
      data <- filtered()
      #we need aes_string since input$xcol isn't inside data
      ggplot(data=data,aes_string(x="Attendance"))+
        geom_histogram()+
        theme_set(theme_gray(base_size = 16))+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
        scale_fill_discrete(palette="Blues")
    })
  })

# Run the application on the Local Host
shinyApp(ui = ui, server = server)

# rsconnect::deployApp("C:/R/Jays/Attendance_Dashboard")


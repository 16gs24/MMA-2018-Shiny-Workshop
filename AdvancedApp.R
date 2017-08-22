#load libraries
library(shiny)
library(tidyverse)

#begin by building out UI. We will use a sidebar for selections and 
# a main bar that has a scatterplot, summary stats and the head
ui <- shinyUI(
  #lets make dynamic pages
  fluidPage(
    #build out our side panel
    sidebarLayout(
      sidebarPanel(
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
          selected = unique(basic$Weekday))
      ) ,
      #enough inputs. what outputs do we want to look at.
      mainPanel(tabsetPanel(
        tabPanel("Visualization",(plotOutput('scatter')),
        #Show Summary stats
        verbatimTextOutput('summary')),
        tabPanel("Univariate Analysis",
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
      ggplot(data=data,aes_string(x=input$xcol, 
                                  y='Attendance',
                                  col=input$color))+  
        theme_set(theme_gray(base_size = 16))+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
        scale_fill_discrete(palette="Blues")+
        geom_point()+
        scale_y_continuous(limits=c(20000,41000))
    })
    
    output$Xhistogram <- renderPlot({
      data <- filtered()
      
      #we need aes_string since input$xcol isn't inside data
      ggplot(data=data,aes_string(x=input$xcol))+
        geom_histogram()+  
        theme_set(theme_gray(base_size = 16))+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
        scale_fill_discrete(palette="Blues")
    })
    
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


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
        selectInput('xcol', 'X Variable', names(basic)),
        #Input for the color of the scatterplot
        selectInput('color','Color By',names(basic)),
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
      mainPanel(
                plotOutput('scatter'),
        #Show Summary stats
        verbatimTextOutput('summary'),
        #show the head of our data
        tableOutput('raw')
      )
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
      str(data)
    })
    
    #finally build a ggplot with scatter plot of Attendance vs our choice
    output$scatter <- renderPlot({
      data <- filtered()
      
      #we need aes_string since input$xcol isn't inside data
      ggplot(data=data,aes_string(x=input$xcol, 
                                   y='Attendance',
                                   col=input$color))+
        geom_point()
      })
})

# Run the application on the Local Host
shinyApp(ui = ui, server = server)

# rsconnect::deployApp("C:/R/Jays/Attendance_Dashboard")


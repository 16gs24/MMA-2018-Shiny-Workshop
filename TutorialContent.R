library(shiny)
library(tidyverse)

ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        #X column
        selectInput('xcol', 'X Variable', names(basic)),
        #Color column
        selectInput('color','Color By',names(basic)),
        #month slider
        sliderInput(inputId='month',
                    label="Choose Months to View",
                    value=c(4,10),min=3,max=11),
        #checkbox for day of week
        checkboxGroupInput(
          inputId='dayofweek',
          label='Day of Week',
          choices = unique(basic$Weekday),
          selected = unique(basic$Weekday))
      ) ,
      mainPanel(
                plotOutput('scatter'),
        #Show Summary stats
        verbatimTextOutput('summary'),
        tableOutput('raw')
      )
    )
  )
)

# Server Code
server <-   shinyServer(
  function(input, output) {
    
    filtered <- reactive({
      basic %>%
        filter(
            Weekday %in% input$dayofweek &
            Month %in% input$month)
      })
      
    output$summary <- renderPrint({
      data <- filtered()
      summary(data$Attendance)
    })
    output$raw <- renderTable({
      data <- filtered()
      head(data,8)
    })
    output$scatter <- renderPlot({
      data <- filtered()
      ggplot(data=data,aes_string(x=input$xcol,
                                   y='Attendance',
                                   col=input$color))+
        geom_point()
      })
})

# Run the application on the Local Host
shinyApp(ui = ui, server = server)

# rsconnect::deployApp("C:/R/Jays/Attendance_Dashboard")


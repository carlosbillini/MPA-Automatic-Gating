#Learning Shiny

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)

# Make the app with Dashboard
## Dashboard interface ##

ui = dashboardPage(
  dashboardHeader(title = "Screen List"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      # FIRST COLUMN 
      column( width = 6,
              # PLOT OF THE 2D GRAPH   
             tabBox(title = "2D Plot",
                    width = NULL,
                    tabPanel(title = "Plot",
                             plotOutput("plot",
                                        width = "750px",
                                        height = "550px")),
                    tabPanel(title = "Hits",
                             tableOutput("contents"))
                    ),
              
              # SLIDER BOX NAMED MODIFIERS
            box(title = "Modifiers",
                status = "primary",
                width = NULL,
                solidHeader = TRUE,
                collapsible = TRUE,
                #Text as helper
                p(helpText("Needs to change to SD")),
                #Slider
                sliderInput("range", 
                            label = "Threshold:",
                            min = 0, max = 10, value = 5, step = 0.1)
                )
            ),
      
      # SECOND COLUMN  
      column(width = 6,  
             # ADD FILE
             box(
               title = "Add File",
               status = "warning", 
               width = NULL,
               solidHeader = TRUE, 
               collapsible = TRUE,
               collapsed = TRUE,
               fileInput(
                 "file1", 
                 label = h3("ForeCyt Data"),
                 multiple = TRUE,
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select number of rows to display ----
               radioButtons("disp", "Display",
                            choices = c(Head = "head",
                                        All = "all"),
                            selected = "head"),
               br(), 
               tags$hr(),
               h3("Submit"),
               downloadButton("pass", "Pass"),
               br(),
               br(),
               p("Integral Mol Logo, TM")
             )
      )
    )
  )
)


## Server Functions ##

server = function(input, output) {
  
  thedata <- reactive(df7)
  
  output$plot <- renderPlot(
    
    ggplot(df6, aes(x=S_N.x, y=S_N.y, label=symbol)) + 
      geom_label_repel(aes(
        label = ifelse(S_N.y>(input$range * sd.y + Noise.y) & S_N.x>(input$range * sd.x + Noise.x),as.character(symbol),'')
      ),
      box.padding   = 0.35, 
      point.padding = 0.5,
      segment.color = 'grey50')  +
      geom_point(color = dplyr::case_when(df6$S_N.x > (input$range * sd.x + Noise.x) & df6$S_N.y > (input$range * sd.y + Noise.y) ~ "#d95f02",
                                          TRUE ~ "#7570b3"), size = 3, alpha = 0.8) +
      geom_hline(yintercept = input$range * sd.y + Noise.y, linetype="dashed", color = "red") +
      geom_vline(xintercept = input$range * sd.x + Noise.x, linetype="dashed", color = "red")
  )
  
  output$contents <- renderTable({
    df7 <- filter(df6, S_N.x > (input$range * sd.x + Noise.x) & S_N.y > (input$range * sd.y + Noise.y))
    df7
  })
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste("iris_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(thedata(df7), file)
    }
  )
  
### SPACE TO TEST UPLOAD ###
  
  
   
}

## Run App ##

shinyApp(ui = ui, server = server)
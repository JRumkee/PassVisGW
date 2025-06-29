# Load packages
rm(list = ls())
library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(readxl)

options(dplyr.summarise.inform = FALSE)


# Load data
file = "pelmo_v_6_6_4_Run_summary_and_results.xlsx"
myData <- read_excel(file, "Full Results", skip = 2)
datanames <- colnames(myData)

# Define UI

ui <- page_sidebar(
  sidebar = sidebar(
    fileInput("file1", "Choose SUPER Output File:", accept = ".xlsx"),
    # Select variable for y-axis0.1
    selectInput(
      inputId = "rate",
      label = "Application Rate:",""
      
    ),
    selectInput(
      inputId = "freq",
      label = "Application Frequency:", ""
    ),
    # Select variable for x-axis
    #  selectInput(
    #    inputId = "met",
    #    label = "Substance:",
    #    choices = unique(data$substance_evaluated),
    #    selected = unique(data$substance_evaluated)[1]
    #  ),
    
    numericInput(
      "limit",
      "Pass Threshold: ",
      0.1
    ),
    selectInput(
      inputId = "cnt",
      label = "Country:",
      choices = c("All","AT","BG","BE","HR","CY","CZ","DK","EE","FI","FR","DE","EL",
                  "HU","IE","IT","LV","LU","LT","MT","NL","NO","PL","RO","PT",
                  "SI","SK","ES","SE","GB","Northern Zone","Central Zone","Southern Zone"),
      selected = "All"
    )
    
  ),
  # Output: Show scatterplot
  card(plotOutput(outputId = "scatterplot")),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
)

scenarios = c("Chateaudun", "Hamburg","Jokioinen", "Kremsmuenster",
              "Okehampton", "Piacenza", "Porto","Sevilla","Thiva")
# Define server
CR = list("All" = c(1,2,3,4,5,6,7,8,9),
          "AT" = c(1,2,4,5),
          "BE"=c(1,2,4,5),
          "HR"=c(1,2,3,4,5,6,7,8,9),
          "CZ"=c(2,4),
          "DK" =c(2),
          "EE" = c(2,3),
          "FI" = c(2,3),
          "FR" = c(1,2,3,4,5,6,7,8,9),
          "DE" = c(2,4),
          "HU" = c(1,2,4,6,7),
          "IE" = c(2,4,5),
          "IT" = c(1,2,6,9),
          "LV" =c(2,3),
          "LT" =c(2),
          "NL" = c(4),
          "NO" = c(2),
          "RO" = c(5,7),
          "PT" = c(6,7,8,9),
          "SI" = c(1,2,4,5,6),
          "SK"=c(1,2,4),
          "ES" = c(1,2,6,7,8,9),
          "SE" = c(2),
          "GB" = c(1,2,4,5),
          "Northern Zone" = c(2,3),
          "Central Zone" = c(1,2,4,5,6,7),
          "Southern Zone" = c(1,2,3,4,5,6,7,8,9)
)



server <- function(input, output, session) {
  
  
  data <- reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      d <- myData
    } else {
      d <- read_xlsx(inFile$datapath, "Full Results", skip = 2)
      colnames(d) = datanames
    }
    d
  })
  
  plot_subset =  reactive({data() %>% group_by(Application.BBCH, scenario,Crop,Application.Rate,Application.Frequency,substance_evaluated) %>%
      summarise(Outcome = if(pec_gw <= input$limit)"Pass" else "Fail") %>%
      filter( Application.Rate==input$rate,Application.Frequency==input$freq, scenario %in% scenarios[CR[[input$cnt]]])
    
  })
  
  output$scatterplot <- renderPlot({ 
    
    
    ggplot( data = plot_subset(), aes(x = substance_evaluated, y = scenario)) +
      geom_tile(aes(fill = Outcome), color = "black")+ scale_fill_manual(values = c("Pass" = "blue", "Fail" = "red"))+
      xlab("Substance")+ ylab("Scenario") + facet_wrap(.~Application.BBCH)
    
  })
  
  observe({
    updateSelectInput(session, "rate",
                      label = "Rate",
                      choices = unique(data()$Application.Rate),
                      selected = unique(data()$Application.Rate[1]))
    
    updateSelectInput(session, "freq",
                      label = "Frequency",
                      choices = unique(data()$Application.Frequency),
                      selected = unique(data()$Application.Frequency[1]))
  })
  
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)
